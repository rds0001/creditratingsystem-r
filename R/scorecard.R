fit_scorecard <- function(design_matrix,outcomes,feature_order,fit_spec,target,training,model_id,model_version,weights=NULL,max_iter=100,tolerance=1e-10,context=NULL) {
 if(!.cr_object(fit_spec)).cr_error("INVALID_SCORECARD_SPEC","Feature kinds and neutral profile required")
 neutral<-.cr_numeric(unlist(fit_spec$neutral_profile),"neutral_profile",vector_only=TRUE);kinds<-unlist(fit_spec$feature_kinds)
 if(length(neutral)!=length(feature_order)||!is.character(kinds)||length(kinds)!=length(feature_order)||any(!kinds %in% c("woe","expert"))).cr_error("INVALID_SCORECARD_SPEC","One neutral and kind per feature required")
 fitted<-fit_binary_glm(design_matrix,outcomes,link="logit",feature_order=feature_order,fit_intercept=TRUE,target=target,training=training,model_id=model_id,model_version=model_version,weights=weights,max_iter=max_iter,tolerance=tolerance,context=context)
 model<-fitted$model;model$method_id<-"scorecard.fit_scorecard"
 x<-.cr_numeric(design_matrix,"design_matrix");rows<-lapply(seq_len(nrow(x)),function(i)as.list(x[i,]))
 model$scorecard_spec<-list(neutral_profile=as.list(neutral),feature_kinds=as.list(kinds),design_status="CALLER_FROZEN",design_fingerprint=.cr_hash(rows));model<-.cr_model(model)
 .cr_fit_result(model,"scorecard.fit_scorecard",model$diagnostics,context)
}
score_scorecard <- function(fitted_model,transformed_features,feature_order,expected_target,context=NULL) {
 model<-.cr_model(fitted_model)
 if(!identical(model$method_id,"scorecard.fit_scorecard")||!identical(model$parameters$link,"logit")).cr_error("UNSUPPORTED_MODEL","Logistic scorecard artifact required")
 copy<-model;copy$method_id<-"classification.fit_binary_glm"
 prediction<-predict_pd(copy,transformed_features,feature_order,expected_target,context=context)
 x<-.cr_numeric(transformed_features,"transformed_features");beta<-.cr_numeric(unlist(model$parameters$coefficients),"coefficients",vector_only=TRUE);neutral<-.cr_numeric(unlist(model$scorecard_spec$neutral_profile),"neutral_profile",vector_only=TRUE)
 if(length(beta)!=ncol(x)+1L||length(neutral)!=ncol(x)).cr_error("INVALID_MODEL","Coefficient or neutral dimensions differ")
 contributions<-sweep(x,2,beta[-1],"*");eta<-beta[1]+rowSums(contributions);reference<-neutral*beta[-1];differences<-sweep(contributions,2,reference,"-")
 if(any(!is.finite(eta))||any(!is.finite(contributions))).cr_error("NUMERICAL_RANGE","Scorecard contribution overflow")
 .cr_result(list(pd=prediction$value,log_odds=eta,intercept=beta[1],feature_contributions=contributions,reference_contributions=reference,contribution_differences=differences,feature_order=as.list(feature_order)),"scorecard.score_scorecard","log_odds_default_over_nondefault",context,
  diagnostics=list(model_id=model$model_id,model_version=model$model_version,target=model$target,neutral_profile=as.list(neutral),refitted=FALSE))
}
scale_score_points <- function(log_odds,base_score,base_odds,points_to_double_odds,odds_convention,score_direction,context=NULL) {
 eta<-.cr_numeric(log_odds,"log_odds",vector_only=TRUE)
 scalar<-function(x,name){x<-.cr_numeric(x,name,vector_only=TRUE);if(length(x)!=1L).cr_error("INVALID_INPUT",paste(name,"must be scalar"));as.numeric(x)}
 base<-scalar(base_score,"base_score");odds<-scalar(base_odds,"base_odds");pdo<-scalar(points_to_double_odds,"points_to_double_odds")
 choice<-function(x,allowed)is.character(x)&&length(x)==1L&&!is.na(x)&&x %in% allowed
 if(odds<=0||pdo<=0||!choice(odds_convention,c("default_over_nondefault","nondefault_over_default"))||!choice(score_direction,c("higher_is_safer","higher_is_riskier"))).cr_error("INVALID_SCORE_SCALE","Positive odds/PDO and explicit conventions required")
 base_eta<-log(odds)*if(odds_convention=="default_over_nondefault")1 else -1
 slope<-pdo/log(2)*if(score_direction=="higher_is_safer")-1 else 1;intercept<-base-slope*base_eta;points<-intercept+slope*eta
 if(!is.finite(intercept)||any(!is.finite(points))).cr_error("NUMERICAL_RANGE","Score scaling overflow")
 .cr_result(list(points=points,inverse=list(score_intercept=intercept,score_slope_per_default_log_odds=slope,formula="pd=expit((points-score_intercept)/score_slope_per_default_log_odds)")),"scorecard.scale_score_points","score_points",context,
  diagnostics=list(base_score=base,base_odds=odds,points_to_double_odds=pdo,odds_convention=odds_convention,score_direction=score_direction,input_log_odds="default_over_nondefault",rounding="none"))
}
apply_qualitative_adjustments <- function(qualitative_inputs,coefficient_spec,neutral_levels,base_value,adjustment_scale,context=NULL) {
 if(!all(vapply(list(qualitative_inputs,coefficient_spec,neutral_levels),.cr_object,logical(1)))||!length(coefficient_spec)).cr_error("INVALID_ADJUSTMENT","Named inputs coefficients and neutral levels required")
 base<-.cr_numeric(base_value,"base_value",vector_only=TRUE)
 if(length(base)!=1L||!is.character(adjustment_scale)||length(adjustment_scale)!=1L||is.na(adjustment_scale)||!adjustment_scale %in% c("log_odds","rating_notches")).cr_error("INVALID_ADJUSTMENT","Scalar base and explicit adjustment scale required")
 contributions<-list()
 for(feature in names(coefficient_spec)) {
  levels<-coefficient_spec[[feature]];value<-qualitative_inputs[[feature]];neutral<-neutral_levels[[feature]]
  if(is.null(value)||length(value)==1L&&is.na(value)).cr_error("MISSING_INPUT","Missing is not neutral")
  if(!.cr_object(levels)||!is.character(value)||length(value)!=1L||!is.character(neutral)||length(neutral)!=1L||is.na(neutral)||!value %in% names(levels)||!neutral %in% names(levels)).cr_error("UNKNOWN_CATEGORY","Observed and neutral levels require coefficients")
  selected<-.cr_numeric(levels[[value]],"coefficient",vector_only=TRUE);reference<-.cr_numeric(levels[[neutral]],"neutral_coefficient",vector_only=TRUE)
  if(length(selected)!=1L||length(reference)!=1L).cr_error("INVALID_ADJUSTMENT","Scalar coefficients required")
  contribution<-selected-reference
  if(adjustment_scale=="rating_notches"&&contribution!=round(contribution)).cr_error("INCOMPATIBLE_UNIT","Notch differences must be integers")
  contributions[[feature]]<-contribution
 }
 total<-sum(unlist(contributions));adjusted<-base+total
 if(!is.finite(adjusted)).cr_error("NUMERICAL_RANGE","Adjustment overflow")
 .cr_result(list(contributions=contributions,total_adjustment=total,adjusted_value=adjusted,adjusted_pd=if(adjustment_scale=="log_odds")stats::plogis(adjusted) else NULL),"scorecard.apply_qualitative_adjustments",adjustment_scale,context,
  diagnostics=list(neutral_levels=neutral_levels,missing_is_neutral=FALSE),limitations="Rating notches are not log-odds; notch results require a separately declared rating scale and boundary policy.")
}
scorecard_reason_codes <- function(score_contributions,reference_profile,reason_policy,context=NULL) {
 if(!all(vapply(list(score_contributions,reference_profile,reason_policy),.cr_object,logical(1)))||!setequal(names(score_contributions),names(reference_profile))||!length(score_contributions)).cr_error("INVALID_REASON_POLICY","Matching named feature contributions required")
 direction<-reason_policy$adverse_direction;limit<-.cr_numeric(reason_policy$max_reasons,"max_reasons",vector_only=TRUE);codes<-reason_policy$reason_codes
 if(!is.character(direction)||length(direction)!=1L||is.na(direction)||!direction %in% c("positive","negative")||length(limit)!=1L||limit<1||limit!=floor(limit)||!.cr_object(codes)||!setequal(names(codes),names(score_contributions))).cr_error("INVALID_REASON_POLICY","Explicit direction count and codes required")
 rows<-list();total<-0
 for(name in sort(names(score_contributions),method="radix")) {
  .cr_string(codes[[name]],"reason code","INVALID_REASON_POLICY")
  contribution<-.cr_numeric(score_contributions[[name]],"contribution",vector_only=TRUE);reference<-.cr_numeric(reference_profile[[name]],"reference",vector_only=TRUE)
  if(length(contribution)!=1L||length(reference)!=1L).cr_error("INVALID_INPUT","Scalar contributions required")
  delta<-contribution-reference;total<-total+delta;adverse<-delta*if(direction=="positive")1 else -1
  rows[[length(rows)+1L]]<-list(feature=name,reason_code=codes[[name]],difference=delta,adverse_magnitude=adverse)
 }
 if(!is.finite(total)).cr_error("NUMERICAL_RANGE","Contribution difference overflow")
 selected<-Filter(function(r)r$adverse_magnitude>0,rows)
 if(length(selected)){ord<-order(-vapply(selected,function(r)r$adverse_magnitude,numeric(1)),vapply(selected,function(r)r$feature,character(1)),method="radix");selected<-head(selected[ord],limit)}
 .cr_result(list(reasons=selected,all_differences=rows,total_difference=total,unreported_difference=total-sum(vapply(selected,function(r)r$difference,numeric(1)))),"scorecard.scorecard_reason_codes","contribution_differences",context,
  diagnostics=list(adverse_direction=direction,tie_break="feature_name_lexicographic",causal_interpretation=FALSE))
}
