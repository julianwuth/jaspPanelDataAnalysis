import QtQuick
import JASP.Controls
import JASP

Group
{
	VariablesForm
	{
		AvailableVariablesList { name: "allVariables" }

		AssignedVariablesList
		{
			name:			"dependent"
			label:			qsTr("Dependent variable")
			info:			qsTr("The outcome variable that is modelled. Panel models take a single dependent variable.")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"covariates"
			label:			qsTr("Covariates")
			info:			qsTr("Continuous predictors entering the model.")
			singleVariable:	false
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"factors"
			label:			qsTr("Factors")
			info:			qsTr("Categorical predictors entering the model; they are expanded into dummy variables.")
			singleVariable:	false
			allowedColumns:	["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name:			"id"
			label:			qsTr("ID")
			info:			qsTr("The variable identifying the individuals (the cross-sectional dimension of the panel).")
			singleVariable:	true
			allowedColumns:	["nominal", "ordinal"]
		}

		AssignedVariablesList
		{
			name:			"time"
			label:			qsTr("Time")
			info:			qsTr("The variable identifying the time periods (the longitudinal dimension of the panel).")
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
		info:		qsTr("If checked, only the ID variable needs to be specified. The time index is then derived from the row order within each individual, which assumes the data are sorted correctly.")
	}
}
