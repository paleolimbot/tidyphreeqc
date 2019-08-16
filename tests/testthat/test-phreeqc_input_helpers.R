test_that("pH/pe_fix_definition is created correctly", {

  expect_s3_class(phr_pH_fix_definition(), "phr_input_section")
  expect_s3_class(phr_pe_fix_definition(), "phr_input_section")
  expect_s3_class(phr_pH_fix_definition(), "phr_input_section_PHASES")
  expect_s3_class(phr_pe_fix_definition(), "phr_input_section_PHASES")

  expect_length(phr_pH_fix_definition(), 4)
  expect_length(phr_pe_fix_definition(), 4)
})

test_that("pH/pe_fix is created correctly", {

  expect_s3_class(phr_pH_fix(7), "phr_input_section")
  expect_s3_class(phr_pe_fix(7), "phr_input_section")
  expect_s3_class(phr_pH_fix(4), "phr_input_section_EQUILIBRIUM_PHASES")
  expect_s3_class(phr_pe_fix(4), "phr_input_section_EQUILIBRIUM_PHASES")

  expect_length(phr_pH_fix(7), 4)
  expect_length(phr_pe_fix(4), 4)
})

test_that("pH/pe_fix does indeed fix pH/pe", {

  # Fixing pH
  test_pH <- 3
  sol <- phr_solution(pH = 7)
  pH_def <- phr_pH_fix_definition()
  pH_set <- phr_pH_fix(pH = test_pH)
  sel <- phr_selected_output(pH = TRUE)

  res_pH <- phr_run(
    phr_input(sol, pH_def, pH_set, sel)
  ) %>%
    tibble::as_tibble()

  # Fixing pe
  test_pe <- 8
  sol <- phr_solution(pe = 2)
  pe_def <- phr_pe_fix_definition()
  pe_set <- phr_pe_fix(pe = test_pe)
  sel <- phr_selected_output(pe = TRUE)

  res_pe <- phr_run(
    phr_input(sol, pe_def, pe_set, sel)
  ) %>%
    tibble::as_tibble()

  expect_equal(res_pH[res_pH$state == "react",][["pH"]][1], test_pH)
  expect_equal(res_pe[res_pe$state == "react",][["pe"]][1], test_pe)
})

test_that("phr_mix is created correctly", {

  # Creating a test example
  solution_1 <- phr_solution(.number = 1, Na = 1, Cl = 1)
  solution_2 <- phr_solution(.number = 2, K = 1, Cl = 1)
  testmix <- phr_mix("1" = 0.25, "2" = 0.75)

  expect_s3_class(testmix, "phr_input_section")
  expect_s3_class(testmix, "phr_input_section_MIX")
  expect_length(testmix, 4)

})


test_that("phr_mix is working in a model", {

  # Creating a test example
  solution_1_Na <- phr_solution(.number = 1, Na = 1, Cl = 1, units = "mol/kgw")
  solution_2_K <- phr_solution(.number = 2, K = 1, Cl = 1, units = "mol/kgw")
  solution_3_Li <- phr_solution(.number = 3, Li = 1, Cl = 1, units = "mol/kgw")

  mix1_Na <- 0.4
  mix1_K <- 0.6


  mix2_Na <- 0.2
  mix2_K <- 0.3
  mix2_Li <- 0.5

  mix1 <- phr_mix(
    "1" = mix1_Na,
    "2" = mix1_K
  )

  mix2 <- phr_mix(
    "1" = mix2_Na,
    "2" = mix2_K,
    "3" = mix2_Li
  )

  mixrun_1 <- phr_run(
    phr_input(
      solution_1_Na,
      solution_2_K,
      mix1,
      phr_selected_output(totals = c("Na", "K"))
    )
  ) %>% tibble::as_tibble() %>%
    dplyr::filter(state == "react")

  mixrun_2 <- phr_run(
    phr_input(
      solution_1_Na,
      solution_2_K,
      solution_3_Li,
      mix2,
      phr_selected_output(totals = c("Na", "K", "Li"))
    )
  ) %>% tibble::as_tibble() %>%
    dplyr::filter(state == "react")

  # The actual testing of the results
  expect_equal(mixrun_1$`Na(mol/kgw)`[1], mix1_Na)
  expect_equal(mixrun_1$`K(mol/kgw)`[1], mix1_K)
  expect_equal(mixrun_2$`Na(mol/kgw)`[1], mix2_Na)
  expect_equal(mixrun_2$`K(mol/kgw)`[1], mix2_K)
  expect_equal(mixrun_2$`Li(mol/kgw)`[1], mix2_Li)


})

test_that("phr_reaction is created correctly", {
  explitic_reaction <- phr_reaction(
    HCl = 1,
    Reaction_amount = c(1, 2, 3, 3, 3)
  )

  implicit_reaction <- phr_reaction(
    HCl = 2,
    Reaction_amount = 2,
    Linear_steps = 10
  )

  expect_s3_class(explitic_reaction, "phr_input_section")
  expect_s3_class(implicit_reaction, "phr_input_section")
  expect_s3_class(explitic_reaction, "phr_input_section_REACTION")
  expect_s3_class(explitic_reaction, "phr_input_section_REACTION")

  expect_length(explitic_reaction, 4)
  expect_length(explitic_reaction, 4)
})

test_that("phr_reaction switches from implicit to explicit correctly", {
  expect_error(
    phr_reaction(
      HCl = 1,
      Reaction_amount = c(1, 2, 3, 3, 3),
      Linear_steps = 5
    )
  )

  expect_error(
    phr_reaction(
      HCl = 1,
      Reaction_amount = "A",
      Linear_steps = 5
    )
  )
})

test_that("phr_reaction unit handling works", {
  expect_error(phr_reaction(HCl = 1, Reaction_amount = c(1, 2), units = "moles/kgw"))
  expect_error(phr_reaction(HCl = 1, Reaction_amount = c(1, 2), units = "ultramoles"))

  expect_s3_class(phr_reaction(HCl = 1, Reaction_amount = c(1, 2), units = "moles"), "phr_input_section_REACTION")
  expect_s3_class(phr_reaction(HCl = 1, Reaction_amount = c(1, 2), units = "millimoles"), "phr_input_section_REACTION")
  expect_s3_class(phr_reaction(HCl = 1, Reaction_amount = c(1, 2), units = "micromoles"), "phr_input_section_REACTION")

  test_moles <- phr_reaction(HCl = 1, Reaction_amount = c(1), units = "moles")
  test_millimoles <- phr_reaction(HCl = 1, Reaction_amount = c(1), units = "millimoles")
  test_micromoles <- phr_reaction(HCl = 1, Reaction_amount = c(1), units = "micromoles")

  testrun <- purrr::map_dfr(
    list(test_moles, test_millimoles, test_micromoles),
    function(x) {
      phr_run(
        x,
        phr_solution(pH = 7),
        phr_selected_output(totals = "Cl")
      ) %>%
        tibble::as_tibble()
    }
  ) %>% dplyr::filter(state == "react")

  expect_equal(testrun[["Cl(mol/kgw)"]][1], 1)
  expect_equal(testrun[["Cl(mol/kgw)"]][2], 0.001)
  expect_equal(testrun[["Cl(mol/kgw)"]][3], 0.000001)
})

test_that("phr_reaction modelling works both implicit and explicit", {
  explicit_sequence <- c(5, 3, 3, 9, 2, 7)
  explicit_run <- phr_run(
    phr_reaction(HCl = 1, Reaction_amount = explicit_sequence),
    phr_solution(pH = 7),
    phr_selected_output(totals = "Cl")
  ) %>%
    tibble::as_tibble() %>%
    dplyr::filter(state == "react")

  implicit_sequence <- seq(2 / length(explicit_sequence), 2, length.out = length(explicit_sequence))
  implicit_run <- phr_run(
    phr_reaction(HCl = 1, Reaction_amount = 2, Linear_steps = length(explicit_sequence)),
    phr_solution(pH = 7),
    phr_selected_output(totals = "Cl")
  ) %>%
    tibble::as_tibble() %>%
    dplyr::filter(state == "react")

  expect_equal(explicit_run$`Cl(mol/kgw)`, explicit_sequence)
  expect_equal(implicit_run$`Cl(mol/kgw)`, implicit_sequence)
  expect_length(explicit_run$`Cl(mol/kgw)`, length(implicit_sequence))
  expect_length(implicit_run$`Cl(mol/kgw)`, length(explicit_sequence))
})
