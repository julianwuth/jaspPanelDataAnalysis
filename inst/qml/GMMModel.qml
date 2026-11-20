import QtQuick
import JASP.Controls
import JASP

import './Common' as Common

// Dynamic panel estimation: lags of the dependent variable enter the model and
// are instrumented by their own deeper lags (Arellano-Bond / Blundell-Bond).
Form
{
	VariablesForm
	{
		AvailableVariablesList { name: "allVariables" }

		AssignedVariablesList
		{
			name:			"dependent"
			label:			qsTr("Dependent variable")
			info:			qsTr("The outcome variable. Its own lags enter the model as regressors and are instrumented by deeper lags.")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"covariates"
			label:			qsTr("Exogenous covariates")
			info:			qsTr("Continuous predictors that are treated as strictly exogenous and instrument themselves. Code categorical predictors as numeric dummy variables, leaving out a reference category.")
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"endogenousCovariates"
			label:			qsTr("Endogenous covariates")
			info:			qsTr("Predictors that are correlated with the error term; they are instrumented by their own lags.")
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"id"
			label:			qsTr("ID")
			info:			qsTr("The variable identifying the individuals.")
			singleVariable:	true
			allowedColumns:	["nominal", "ordinal"]
		}

		AssignedVariablesList
		{
			name:			"time"
			label:			qsTr("Time")
			info:			qsTr("The variable identifying the time periods; it determines the lag structure.")
			singleVariable:	true
			allowedColumns:	["ordinal"]
			enabled:		!idOnly.checked
			onEnabledChanged: if (!enabled && count > 0) itemDoubleClicked(0);
		}
	}

	CheckBox
	{
		name:		"idOnly"
		id:			idOnly
		label:		qsTr("ID only")
		checked:	false
		info:		qsTr("If checked, the time index is derived from the row order within each individual.")
	}

	Section
	{
		title: qsTr("Model")

		IntegerField
		{
			name:			"lagsDependent"
			label:			qsTr("Lags of the dependent variable")
			info:			qsTr("How many lags of the dependent variable enter the model as regressors.")
			defaultValue:	1
			min:			1
			max:			10
		}

		Group
		{
			title: qsTr("GMM Instruments")

			IntegerField
			{
				id:				instrumentLagMin
				name:			"instrumentLagMin"
				label:			qsTr("Smallest lag of the dependent variable")
				info:			qsTr("The shortest lag of the dependent variable used as an instrument. Lags below two are correlated with the differenced error.")
				defaultValue:	2
				min:			2
				max:			20
			}

			IntegerField
			{
				id:				endogenousLagMin
				name:			"endogenousLagMin"
				label:			qsTr("Smallest lag of the endogenous covariates")
				info:			qsTr("The shortest lag of the endogenous covariates used as an instrument. Two suits covariates correlated with the current error; one is valid only if they are sequentially exogenous.")
				defaultValue:	2
				min:			1
				max:			20
			}

			IntegerField
			{
				name:			"instrumentLagMax"
				label:			qsTr("Largest lag")
				info:			qsTr("The longest lag used as an instrument. A large value uses all available lags.")
				defaultValue:	99
				min:			Math.max(instrumentLagMin.value, endogenousLagMin.value)
				max:			99
			}

			CheckBox
			{
				name:		"collapseInstruments"
				label:		qsTr("Collapse instrument matrix")
				checked:	false
				info:		qsTr("Collapses the instrument matrix into one column per lag distance, which limits instrument proliferation in panels with many periods.")
			}
		}

		RadioButtonGroup
		{
			name:	"gmmSteps"
			title:	qsTr("Estimation")
			info:	qsTr("One-step GMM uses a fixed weighting matrix, two-step GMM reweights using the first-step residuals.")

			RadioButton { value: "onestep";		label: qsTr("One-step"); checked: true	}
			RadioButton { value: "twosteps";	label: qsTr("Two-steps"); id: twoSteps	}
		}

		RadioButtonGroup
		{
			name:	"transformation"
			title:	qsTr("Transformation")
			info:	qsTr("Difference GMM estimates the differenced equation only. System GMM adds the level equation; it requires individual and time effects and assumes that the exogenous covariates are uncorrelated with the individual effects.")

			RadioButton { value: "d";	label: qsTr("Difference GMM"); checked: true	}
			RadioButton { value: "ld";	label: qsTr("System GMM")						}
		}

		// pgmm only accepts individual or two-ways effects
		Common.Effects { allowTime: false }
	}

	Section
	{
		title: qsTr("Statistics")

		// one-step standard errors are always robust
		CheckBox
		{
			name:		"robustSE"
			label:		qsTr("Windmeijer-corrected standard errors")
			checked:	true
			enabled:	twoSteps.checked
			info:		qsTr("Applies the Windmeijer finite-sample correction to the two-step standard errors, which are otherwise far too small. One-step standard errors are always robust.")
		}

		Common.Plot{}
	}
}
