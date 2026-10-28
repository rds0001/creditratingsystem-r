# Offline synthetic illustrations using only the installed native R package.
library(creditratingsystem)

cat("Synthetic algebraic examples; amounts use the same monetary unit.\n")
context <- build_calculation_context(data_manifest = list(
    data_origin = "SYNTHETIC", parameter_origin = "SYNTHETIC_REFERENCE",
    dataset_id = "bootstrap_handcrafted"))
loss <- expected_loss(pd = 0.02, lgd = 0.4, ead = 1000, context = context)
cat("Expected default loss:", loss$value, "\n")
cat("Raw economic LGD examples:",
    expected_loss(c(0.2), c(-0.25, 1.5), c(100), context = context)$value, "\n")

hazards <- c(0.1, 0.2)
cumulative <- cumulative_pd_from_conditional(hazards, context = context)
marginal <- marginal_pd_from_conditional(hazards, context = context)
cat("Cumulative first-default PD:", cumulative$value, "\n")
cat("Marginal first-default PD:", marginal$value, "\n")
cat("Method:", cumulative$method_id, "\n")
cat("Unit:", cumulative$unit, "\n")
cat("First-default masses with absorption:",
    marginal_pd_from_conditional(c(0.25, 1, 0.9), context = context)$value, "\n")
cat("Tiny hazards:",
    cumulative_pd_from_conditional(c(1e-20, 2e-20), context = context)$value, "\n")
