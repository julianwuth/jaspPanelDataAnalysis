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
			name:	"pgglsEstimator"
			title:	qsTr("Model")
			info:	qsTr("The transformation applied before the unrestricted error covariance matrix is estimated.")

			RadioButton { value: "within";	label: qsTr("Fixed"); checked: true	}
			RadioButton { value: "pooling";	label: qsTr("Pooling")				}
			RadioButton { value: "fd";		label: qsTr("First-Difference")		}
		}

		// pggls only accepts one-way effects
		Common.Effects { allowTwoways: false }

		Common.Plot{}
	}
}
