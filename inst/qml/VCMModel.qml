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

		RadioButtonGroup
		{
			name:	"vcmEstimator"
			title:	qsTr("Model")
			info:	qsTr("The no-pooling model estimates a separate regression per group; the random coefficients model shrinks them towards a common mean (Swamy).")

			RadioButton { value: "within";	label: qsTr("No pooling"); checked: true	}
			RadioButton { value: "random";	label: qsTr("Random coefficients")			}
		}

		// pvcm only accepts one-way effects
		Common.Effects { allowTwoways: false }

		Common.Plot{}
	}
}
