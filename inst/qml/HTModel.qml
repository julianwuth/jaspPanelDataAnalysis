import QtQuick
import JASP.Controls
import JASP

import './Common' as Common

// The Hausman-Taylor estimator needs every regressor classified along two axes:
// whether it varies over time, and whether it is correlated with the individual
// effect. The exogenous regressors serve as their own instruments, and the
// time-varying ones additionally instrument the endogenous time-invariant
// regressors. The estimator is only defined for individual effects.
Form
{
	VariablesForm
	{
		AvailableVariablesList { name: "allVariables" }

		AssignedVariablesList
		{
			name:			"dependent"
			label:			qsTr("Dependent variable")
			info:			qsTr("The outcome variable that is modelled.")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:	"timeVaryingExogenous"
			label:	qsTr("Time-varying exogenous")
			info:	qsTr("Regressors that vary over time and are uncorrelated with the individual effect. These instrument the endogenous regressors.")
		}

		AssignedVariablesList
		{
			name:	"timeInvariantExogenous"
			label:	qsTr("Time-invariant exogenous")
			info:	qsTr("Regressors that are constant within individuals and uncorrelated with the individual effect.")
		}

		AssignedVariablesList
		{
			name:	"timeInvariantEndogenous"
			label:	qsTr("Time-invariant endogenous")
			info:	qsTr("Regressors that are constant within individuals and correlated with the individual effect. At least as many time-varying exogenous regressors are needed to identify them.")
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
			info:			qsTr("The variable identifying the time periods.")
			singleVariable:	true
			allowedColumns:	["ordinal", "nominal"]
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
		title: qsTr("Statistics")

		Common.Coefficients{}

		DropDown
		{
			name:	"htMethod"
			label:	qsTr("Instrument set")
			info:	qsTr("The set of instruments constructed from the exogenous regressors.")
			values:
			[
				{ label: qsTr("Hausman-Taylor"),			value: "baltagi"	},
				{ label: qsTr("Amemiya-MaCurdy"),			value: "am"			},
				{ label: qsTr("Breusch-Mizon-Schmidt"),		value: "bms"		}
			]
		}

		Common.Plot{}
	}
}
