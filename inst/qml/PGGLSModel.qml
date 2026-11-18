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
			info:	qsTr("The transformation applied before the unrestricted error covariance matrix is estimated. The matrix requires many more groups than time periods (or, with time effects, than individuals).")

			RadioButton { value: "within";	label: qsTr("Fixed"); checked: true	}
			RadioButton { value: "pooling";	label: qsTr("Pooling")				}
			RadioButton { value: "fd";		label: qsTr("First-Difference"); enabled: pgglsEffects.value !== "time"	}
		}

		// pggls only accepts one-way effects
		Common.Effects { id: pgglsEffects; allowTwoways: false }

		Common.Plot{}
	}
}
