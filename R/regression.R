#' @title The Regression Game
#'
#' @description
#' The \code{regression} function is used for solving problems in the data-based
#' game "The regression Game".
#'
#' @param ... \code{regression} function is called with different arguments, which
#' vary depending on a problem that Beta and Bit are trying to solve. See
#' \code{Details} in order to learn more about the list of possible arguments.
#'
#' @details Every time when some additional hints are needed one should add
#' \code{hint = TRUE} or \code{techHint = TRUE} argument to the \code{regression} function.
#' Technical hints will point out R packages and/or functions which might help
#' you to solve the task while "normal" hints provide you with methodological
#' advices.
#'
#' In this game you are helping a Professor Pearson.
#' You can communicate with him through the \code{regression} function.
#'
#' In each call include the \code{subject} parameter (indicating which task
#' you are trying to answer) and the \code{content} parameter (providing
#' information professor Pearson is asking you for in a given task).
#'
#' Data used in the game comes from the study of Polish upper-secondary
#' schools first grade students. It was conducted together with the
#' PISA 2009 study using the same cognitive tests and questionnaires as
#' in PISA 2009 but on a different group of students (in Poland most of the
#' students in a PISA sample attends lower-secondary schools). The students who
#' participated in the first wave of the study were followed in the 2nd grade of
#' upper-secondary school within the research program \emph{Our further study
#' and work} (\emph{Nasza Dalsza Nauka i Praca}). Both studies were conducted by
#' the Institute of Philosophy and Sociology Polish Academy of Sciences.
#' \strong{The original data was changed a little, to better fit the purpose of
#' the game.}
#'
#' "The Regression Game" is a free of charge, educational project of the
#' SmarterPoland.pl Foundation.
#' @return
#' Function returns one of three possible values:
#' \itemize{
#'   \item{\code{TRUE} if you provided correct answer to a task,}
#'   \item{\code{FALSE} if you provided wrong answer to a task,}
#'   \item{\code{NULL} if function can't identify task you wanted to answer.}
#' }
#' @author
#' Tomasz Żółtak - the idea and the implementation,
#' @examples
#' regression()
#' regression(hint = TRUE)
#' regression(techHint = TRUE)
#' @rdname regression
#' @importFrom stats lm deviance formula
#' @export
regression <- function(...) {
  args = list(...)

  textsRegression = list(
    wrongAnswer = "This doesn't seem to be a correct result.",
    notSoFar = "Solve previous tasks first!",
    regressionInit =
      paste0(
        "During their summer internship Beta and Bit are helping professor Pearson to analyse data from his educational research on Polish upper-secondary schools students.\n",
        "\nProfessor Pearson is mostly interested in educational inequalities, particularly in how the socio-economic status of the parents affects their children's educational attainment. He hopes his research will help to develop programmes providing support to young people from disadvantaged families and reducing inequalities in our society.\n",
        "\nIf you want to help Beta and Bit in their analysis, type:\n",
        "  regression(subject = \"Summer internship\")\n",
        "\nIf you need help, try to add `hint = TRUE` and/or `techHint = TRUE` argument to the `regression()` function call."
      ),
    task1 =
      paste0(
        "Dear Beta and Bit,\n",
        "\nI sent you a dataset named 'dataDNiP' for an analysis. An additional dataset called 'varLabels' contains variable labels so that you can check what each variable measures.\n",
        "\nLet's start with something simple. Can you compute for me the correlations between the measures of cognitive abilities (`MATH_2009`, `READ_2009`, `SCIE_2009`) and the highest parental International Socio-Economic Index `hisei`?\n",
        "\nYou should get three correlations.\n",
        "\nPlease send me back a vector containing these three correlations by calling:\n",
        "  `regression(subject = \"Correlations\", content = \"vector of correlations\")`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task2 =
      paste0(
        "Dear Beta and Bit,\n",
        "\nThe correlations you sent me are very strange. They should be positive and about twice as high! Perhaps there is something wrong with the dataset... I think that I mixed names of the variables when I was preparing the dataset. I'm sure the first sixteen names are correct, but the rest might be a mess.\n",
        "\nCan you try to identify the name of the variable that measures HISEI in the dataset? You should by able to find it because I remember that the relationship between `READ_2009` and HISEI was nearly perfectly linear (although not very strong).\n",
        "\nWhen you're done, please send me back the correct name of the variable that describes HISEI by calling:\n",
        "  `regression(subject = \"Name of the variable\", content = \"name of the variable describing HISE\")`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task3 =
      paste0(
        "Good work!\n",
        "\nLuckily, I found the correct version of the dataset. I sent it to you. It is named simply `DNiP`. Please, use it for further analysis.\n",
        "\nNow I want you to help me with analysing the relationship between the students' attainment and the income of their parents. I estimated an OLS regression model in which the reading test score is predicted by two variables: `cultpos` (index describing the availability of cultural resources in a household) and `income` (monthly household income):\n",
        "  `lm(READ_2009 ~ cultpos + income, DNiP)`\n",
        "Although I don't expect the income effect to be strong when the availability of cultural resources is controlled for, it should be statistically significant.\n",
        "\nI think the skewness of the `income` distribution might cause some problems in the model. Perhaps you could propose a (nonlinear) transformation of `income`, so that the transformed variable is more strongly related to `READ_2009` and statistically significant (on a 0.05 significance level) when put instead of the original variable `income` into the regression model described above.\n",
        "\nPlease, send me the expression describing such a transformation by calling:\n",
        "  `regression(subject = \"transformation\", content = expression(your transformation of income))`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task4 =
      paste0(
        "Very good!\n",
        "\ncomment on 3rd task\n",
        "\nPlease look at a somewhat more complicated model. I wanted to examine impact of multiple variables on reading test scores (`READ_2009`). There are variables describing sex and school type (track), and indicators of socio-economic status of student's family (`log(income)`, `homepos`, `hisei`, `csesi`) and of psychological tests scores (`RAVEN_WYN`, `STAI_C_WYN`, `STAI_S_WYN`, `SES_WYN`, `ZAMPS_WYN`). I estimate the model with:\n",
        "  `lm(READ_2009 ~ SEX + SCHOOL_TYPE + log(income) + homepos + hisei + csesi + RAVEN_WYN + STAI_C_WYN + STAI_S_WYN + SES_WYN + ZAMPS_WYN, DNiP)`\n",
        "\nHowever, although each variable alone is a statistically significant predictor of `READ_2009` most of them turns out to be insignificant in this model.\n",
        "\nPerhaps the problem lies in some relationship between the predictors ... it's called \"collinearity\", or something like that... I think that removing some variables from the model will make the remaining ones statistically significant.\n",
        "\nPlease find out which variables should be removed so that the remaining ones are significant at the 0.05 significance level. You should remove as few variables as possible.\n",
        "\nIf you're done, send me the names of the variables to remove by calling:\n",
        "  `regression(subject = \"Collinearity\", content = character_vector_with_names_of_removed_variables)`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task5 =
      paste0(
        "It looks fine!\n",
        "\nI wonder whether you have tried to remove variables of some special properties or have simply written a code that checks all possible combinations of the predictors...\n",
        "\nLet's get back to the relationship between `hisei` and `READ_2009'. I wonder if it looks similar within different schools in the sample. It might be a general relationship or it might depend on a school context.\n",
        "\nCan you provide me a data frame consisting of two columns: `SCHOOL_ID` and `par_hisei`? The first column is self-descriptive and the second one should contain values of the slope parameter for `hisei` (from the OLS regression model) in each school.\n",
        "\nWhen you finish, please send me the data frame by calling:\n",
        "  `regression(subject = \"Groups\", content = data_frame_containig_results`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task6 =
      paste0(
        "Great!\n",
        "\nDid you perform different regressions in each group or used one model with an interaction term between `hisei` and `factor(SCHOOL_ID)`?\n",
        "\nThe variability between schools does not seem to be high with respect to the `hisei` parameter. However let's try to identify schools with bigger differences.\n",
        "\nThe mean value of the slope parameter among schools is about 0.434. Can you identify schools for which the slope parameter value of `hisei` is statistically significantly different (at the 0.05 significance level) from the mean value among schools (0.434)? I want you to do this using the two-sided Wald t test (or the F test, which is equivalent in this case) on the basis of the OLS regression model (or its reparametrisation):\n",
        "  lm(READ_2009 ~ hisei * factor(SCHOOL_ID), DNiP)\n",
        "\nAs a result please send me a vector of `SCHOOL_ID` of the schools for which the slope parameter value is statistically significantly different from mean by calling:\n",
        "  `regression(subject = \"Significant differences\", content = vector_of_school_ids)`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    task7 =
      paste0(
        "Well done!\n",
        "\ncomment on 6th task\n",
        "\nNow let's turn to a little different issue. It's well known that in lower grades the cognitive abilities, as measured by for example in `READ_2009`, depend on pupils' age. At the upper-secondary level this effect is probably much lower or even non-existent but we should check it.\n",
        "\nThere is another issue that complicates the analysis. In the sample there are students who started school one year earlier than they were supposed to (normally children in Poland start primary school at the age of 7 but under certain conditions they can start when they are 6 years old). These pupils are not representative for their cohort because they are selective (they are brighter than the average). On the other hand there are also pupils who are older than they should be. These are pupils who either started school one year later than they were supposed to or repeated a grade. Whatever the reason we can describe them as \"negatively selected\", i.e. they have usually lower cognitive abilities than other pupils of their age (who are not in our sample because when the study was conducted they were already in an upper grade).\n",
        "\nBecause of that the relationship between `RAVEN_AGE` and `READ_2009` is perhaps not continuous: it can look different in some ranges of age than in others. Your task is to check how this relationship looks like and to propose a linear model describing it in a proper way.\n",
        "\nPlease, send me the formula of the model by calling:\n",
        "  `regression(subject = \"Age\", content = READ_2009 ~ your_formula`\n",
        "\nGenerally, your formula should contain transformations of only one variable: `RAVEN_AGE`. If you find it more convenient not to use explicit transformations in the formula (using `I()` or other functions), you can send me a formula containing additional variables you want to create. In such case you must call `regression()` function with an additional argument `vars` that will contain a named list of the expressions describing how to compute the additional variables. Nevertheless you can construct them only as transformations of `RAVEN_AGE`. Here is an example of such a call:\n",
        "  `regression(subject = \"age\", content = READ_2009 ~ RAVEN_AGE + AGE3, vars = list(AGE3 = expression(RAVEN_AGE^3)))`\n",
        "\nBest regards\n\nProfessor Pearson\n"
      ),
    congratulations =
      paste0(
        "Congratulations!\n",
        "You have solved all the problems and helped me analyse the data! I’m sure that your solutions will enable me to understand the causes of educational inequalities better and thus develop effective ways to reduce them.\n",
        "\nI wish you many interesting and challenging analytical problems to solve in the future!\n",
        "\nProfessor Pearson\n"
      ),
    hint0 = "No hint (but there might be a technical one - try `techHint=TRUE`)\n",
    hint1 = "No hint (but there might be a technical one - try `techHint=TRUE`)\n",
    hint2 = "If the relationship is linear, the quality of the prediction from a regression model can't be improved by adding nonlinear terms.\n",
    hint3 = "The distribution of `income` is highly right-skewed while the distribution of `READ_2009` is quite symmetric around mean. Try to find such a transformation that can reduce the right-skewness of `income` (i.e. that can \"shorten its long tail\").\n",
    hint4 = "A combination of different methods is needed to provide the right solution.\n",
    hint5 = "You can try two ways to deal with this: either estimate separate regression model for each school (some automation will be desirable) or estimate one model with an interaction term (remember that in the dataset `SCHOOL_ID` is not given as a factor). In the latter case you will probably have to transform the results a little to get the values you're interested in.\n",
    hint6 = paste0(
      "You can try to reparametrize the regression model by using proper contrasts so that the differences of interest will be model parameters. For a brief introduction to contrasts you can consult http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm.\n",
      "Alternatively you can compute the Wald test for all schools slope coefficients.\n"
    ),
    hint7 = "The best way to see how a complex relationship between two variables generally looks like is to use graphical diagnostics.\n",
    techHint0 = "Just type `regression(subject = \"Summer internship\")` into the console and hit `enter`.\n",
    techHint1 = "Try to use the function `cor()`. Remember to specify the argument `use` when calling it.\n",
    techHint2 = paste0(
      "You can estimate a linear model using the `lm()` function.\n",
      "Coefficients as well as most important goodness-of-fit measures can be obtained by running the `summary()` function on the model object returned by `lm()`."
    ),
    techHint3 = "You can visualise the distribution of a continuous variable using the combination of `plot()` and `density()` functions.\n",
    techHint4 = "The `vif()` function form the `car` package provides a collinearity diagnostics.\n",
    techHint5 = "No hint (but there might be a methodological one - try `hint=TRUE`)\n",
    techHint6 = paste0(
      "You can use the `C(data, contrastFunction)` function to set the variable contrast. There are many ready-to-use functions generating contrasts (look for functions whose names start with `contr.`).\n",
      "If you have decided to compute the Wald test, you can obtain the coefficient values and their standard errors from `summary(model)$coef`.\n"
    ),
    techHint7 = "You can try to use `geom_points()` and `geom_smooth()` from the `ggplot2` package or, if you are not familiar with `ggplot2`, simply use the `plot()` function.\n"
  )

  if (length(args) == 0) {
    cat(textsRegression$regressionInit)
    return(invisible(NULL))
  }
  if ("subject" %in% names(args)) {
    args$subject = tolower(args$subject)
    if (args$subject == "summer internship") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint0)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint0)
        return(invisible(FALSE))
      }
      cat(textsRegression$task1)
      return(invisible(TRUE))
    } else if (!any(c("content", "hint") %in% names(args))) {
      cat("You forgot to send the results!\nUse argument `content` of the `regression()` function to send the data.\n")
      return(invisible(FALSE))
    }
    # zadanie 1. - korelacje
    if (args$subject == "correlations") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint1)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint1)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric` and length of 3.\n")
        return(invisible(FALSE))
      } else if (!is.numeric(args$content) | length(args$content) != 3) {
        cat("Argument `content` must be a vector of mode `numeric` and length of 3.\n")
        return(invisible(FALSE))
      }
      if (!is.null(names(args$content)) & is.vector(args$content)) {
        args$content = args$content[order(names(args$content))]
      }
      if (all.equal(unname(args$content),
                    unname(zadaniaRegresja::answers[[1]]))[1] %in% TRUE) {
        cat(textsRegression$task2)
        return(invisible(TRUE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 2. - diagnostyka liniowości
    } else if (args$subject == "name of the variable") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint2)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint2)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a character vector of length 1.\n")
        return(invisible(FALSE))
      } else if (!is.character(args$content) | length(args$content) != 1) {
        cat("Argument `content` must be a character vector of length 1.\n")
        return(invisible(FALSE))
      }
      if (all.equal(unname(args$content),
                    zadaniaRegresja::answers[[2]])[1] %in% TRUE) {
        cat(textsRegression$task3)
        return(invisible(TRUE))
      }
      else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 3. - przekształcenie zmiennej niezależnej
    } else if (args$subject == "transformation") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint3)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint3)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must contain an expression.\n")
        return(invisible(FALSE))
      } else if (!is.expression(args$content[1])) {
        cat("Argument `content` must contain an expression.\n")
        return(invisible(FALSE))
      } else if (!all(all.vars(args$content[1]) %in% "income")) {
        cat("There should be no variables other than `income` in your expression.\n")
        return(invisible(FALSE))
      }
      incomeTr = tryCatch(
        eval(args$content[1], zadaniaRegresja::DNiP),
        error = function(e) {
          cat(
            "Trying to evaluate your expression: `",
            as.character(args$content)[1],
            "` causes an error:\n\n",
            sep = ""
          )
          print(e)
          return(NULL)
        }
      )
      if (is.null(incomeTr)) {
        return(invisible(FALSE))
      }
      mTemp = with(zadaniaRegresja::DNiP, lm(READ_2009 ~ cultpos + incomeTr))
      if (summary(mTemp)$coef[3, 4] <= 0.05) {
        functionsUsed = setdiff(all.names(args$content[1]), "income")
        if (
          length(functionsUsed) == 1 &
          all(functionsUsed %in% c("log2", "log10"))
        ) {
          commentReplace =
                paste0("It's nice you decided to use logarithmic transformation. ",
                       "The slope parameter for transformed income has clear ",
                       "interpretation: that's the change in prediction when ",
                       "the value of income rises ",
                       ifelse(functionsUsed == "log2", "twice", "ten times"), ".")
        } else if (length(functionsUsed) == 1 &
                   all(functionsUsed %in% "log")) {
          commentReplace =
              paste0(
                "It's nice you decided to use logarithmic transformation. ",
                "However if you used base 2 or 10 instead of e, it would be",
                "a little easier to interpret the slope parameter coefficient",
                "value."
              )
        } else {
          commentReplace = paste0(
            "Note however that if you used base 2 logarithm to ",
            "transform `income`, then the slope parameter would be ",
            "more easily interpretable."
          )
        }
        textsRegression$task4 = sub(
          "comment on 3rd task",
          commentReplace,
          textsRegression$task4
        )
        # trzeba zbadać rozwiązanie i podmienić komentarz do niego
        cat(textsRegression$task4)
        return(invisible(TRUE))
      } else {
        cat("Ufortunately after this transformation `income` is still insignifficant.\n")
        return(invisible(FALSE))
      }
    # zadanie 4. - współliniowość
    } else if (args$subject == "collinearity") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint4)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint4)
        return(invisible(FALSE))
      }
      varsTemp = all.vars(
        ~ SEX + SCHOOL_TYPE + log(income) + homepos + hisei +
        csesi + RAVEN_WYN + STAI_C_WYN +  STAI_S_WYN + SES_WYN + ZAMPS_WYN
      )
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `character`.\n")
        return(invisible(FALSE))
      } else if (!is.character(args$content)) {
        cat("Argument `content` must be a vector of mode `character`.\n")
        return(invisible(FALSE))
      } else if (!all(args$content %in% varsTemp)) {
        cat("Some of variables you gave don't appear in the model. Check variable names.\n")
        return(invisible(FALSE))
      } else if (
        any(sapply(
          zadaniaRegresja::answers[[4]],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        cat(textsRegression$task5)
        return(invisible(TRUE))
      }
      varsTemp = setdiff(varsTemp, args$content)
      varsTemp = sub("income", "log(income)", varsTemp)
      varsTemp = formula(paste("READ_2009 ~ ", paste(varsTemp, collapse = "+")))
      mTemp = lm(varsTemp, zadaniaRegresja::DNiP)
      if (any(summary(mTemp)$coef[-1, 4] > 0.05)) {
        cat("Unfortunately, there is/are still some insignificant parameter(s) in the model.\n")
        print(summary(mTemp))
        return(invisible(FALSE))
      } else {
        cat("All variables in the model are statistically significant, but you removed more variables than in the optimal solution. Try removing other variables.")
      }
    # zadanie 5. - regresja w ramach grup (interakacje I)
    } else if (args$subject == "groups") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint5)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint5)
        return(invisible(FALSE))
      }
      if (!is.data.frame(args$content)) {
        cat("Argument `content` must be a data frame with two columns: `SCHOOL_ID`and `par_hisei`.\n")
        return(invisible(FALSE))
      } else if (!(all(names(args$content) %in% c("SCHOOL_ID", "par_hisei")))) {
        cat("Argument `content` must be a data frame with two columns: `SCHOOL_ID`and `par_hisei`.\n")
        return(invisible(FALSE))
      }
      args$content = args$content[order(args$content$SCHOOL_ID), ]
      if (
        all.equal(
          args$content$par_hisei,
          zadaniaRegresja::answers[[5]]
        )[1] %in% TRUE
      ) {
        cat(textsRegression$task6)
        return(invisible(TRUE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 6. - istotność różnic (interakcje II)
    } else if (args$subject == "significant differences") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint6)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint6)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric`.\n")
        return(invisible(FALSE))
      } else if (!is.numeric(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric`.\n")
        return(invisible(FALSE))
      } else if (!all(args$content %in% zadaniaRegresja::DNiP$SCHOOL_ID)) {
        cat("Some values you gave don't appear in `DNiP$SCHOOL_ID`.\n")
        return(invisible(FALSE))
      } else if (
        any(sapply(
          zadaniaRegresja::answers[[6]],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        if (length(args$content) == length(zadaniaRegresja::answers[[6]][[1]])) {
          commentReplace = "Note however, that you treated the mean value of slope parameters as it was estimated without any error. Do you know what can you do to account for this error while checking significance of the differences?"
        } else {
          commentReplace = "That's nice you took into account that the mean value of slope parameters is also estimated with error."
        }
        textsRegression$task7 = sub(
          "comment on 6th task",
          commentReplace,
          textsRegression$task7
        )
        cat(textsRegression$task7)
        return(invisible(TRUE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 7. - modelowanie wieku
    } else if (args$subject == "age") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint7)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint7)
        return(invisible(FALSE))
      }
      if (!("formula" %in% class(args$content))) {
        cat("Argument `content` must be a model formula.\n")
        return(invisible(FALSE))
      } else if (args$content[[2]] != "READ_2009") {
        cat("There should be simply `READ_2009` on the left side of the model formula given by `content`.\n")
        return(invisible(FALSE))
      }
      if ("vars" %in% names(args)) {
        if (!is.list(args$vars)) {
          cat("Argument `vars` must be a list of expressions.\n")
          return(invisible(FALSE))
        } else if (!all(sapply(args$vars, is.expression))) {
          cat("Argument `vars` must be a list of expressions.\n")
          return(invisible(FALSE))
        } else if (
          !all(sapply(args$vars, function(x) {all(all.vars(x) %in% "RAVEN_AGE")}))
        ) {
          cat("No other variable than `RAVEN_AGE` can appear in expressions given in the `vars` argument.\n")
          return(invisible(FALSE))
        } else if (
          !all(all.vars(args$content) %in% c("READ_2009", "RAVEN_AGE", names(args$vars)))
        ) {
          cat("Expressions defining some variables that appear in model formula given by argument `content` do not appear in argument `vars`. Check your formula and names of elements of the list of expressions.\n")
          return(invisible(FALSE))
        }
        varsTemp = lapply(args$vars, function(x) {
          return(tryCatch(
            eval(x, zadaniaRegresja::DNiP),
            error = function(e) {
              cat(
                "Trying to evaluate your expression: `",
                as.character(x),
                "` causes an error:\n\n",
                sep = ""
              )
              print(e)
              return(NULL)
            }
          ))
        })
        if (any(sapply(varsTemp, is.null))) {
          return(invisible(FALSE))
        }
        dataTemp = cbind(zadaniaRegresja::DNiP, as.data.frame(varsTemp))
      } else {
        if (!all(all.vars(args$content[[3]]) %in% "RAVEN_AGE")) {
          cat("No other variable than `RAVEN_AGE` can appear on the right side the model formula unless you provide expressions describing how to compute them by specifying the `vars` argument.\n")
          return(invisible(FALSE))
        }
        dataTemp = zadaniaRegresja::DNiP
      }
      mTemp = tryCatch(
        lm(args$content, dataTemp),
        error = function(e) {
          cat("Trying to estimate regression model caused an error. Probably there's something wrong with a model formula you provided.\n\n")
          print(e)
          return(NULL)
        }
      )
      if (is.null(mTemp)) {
        return(invisible(FALSE))
      }
      if (deviance(mTemp) <= zadaniaRegresja::answers[[7]]) {
        cat(textsRegression$congratulations)
        return(invisible(TRUE))
      } else {
        cat("Try to change something - your model should fit the data better.")
        return(invisible(FALSE))
      }
    # niepoprawny `subject`
    } else {
      cat("Please check the subject. Something is wrong there!")
      return(invisible(NULL))
    }
  } else if ("hint" %in% names(args)) {
    cat("Just type `regression()` into the console and hit `enter` :)")
    return(invisible(FALSE))
  }

  return(invisible(NULL))
}

