import QtQuick
import JASP.Controls
import JASP

import './Common' as Common

Form
{
	Common.VariableInput{}

	Section
	{
		title: qsTr("Statistics")

		Common.Coefficients{}

		CheckBox
		{
			name:		"randomEffects"
			label:		qsTr("Random effect estimates")
			checked:	false
			info:		qsTr("Display the estimated variance components of the error decomposition.")
		}

		Common.Effects{}

		// the estimator of the variance components changes the whole model, not
		// just the variance component table
		RadioButtonGroup
		{
			name:	"estimators"
			title:	qsTr("Estimators")
			info:	qsTr("The method used to estimate the variance components.")

			RadioButton { value: "swar";	label: qsTr("Swamy-Aurora"); checked: true	}
			RadioButton { value: "walhus";	label: qsTr("Wallace-Hussain")				}
			RadioButton { value: "amemiya";	label: qsTr("Amemiya")						}
			RadioButton { value: "nerlove";	label: qsTr("Nerlove")						}
		}

		Common.Plot{}
	}

	Common.AssumptionChecks{}
}
