import QtQuick
import JASP.Controls
import JASP

Section
{
	title: qsTr("Assumption Checks")

	CheckBox
	{
		name:		"lmTest"
		label:		qsTr("Lagrange multiplier test")
		checked:	false
		info:		qsTr("Breusch-Pagan type test for the presence of random effects in a pooled model.")

		RadioButtonGroup
		{
			id:		lmTestEffects
			name:	"lmTestEffects"
			title:	qsTr("Effects")

			RadioButton { value: "individual";	label: qsTr("Individual"); checked: true	}
			RadioButton { value: "time";		label: qsTr("Time")							}
			RadioButton { value: "twoways";		label: qsTr("Individual and Time")			}
		}

		RadioButtonGroup
		{
			name:	"lmTestType"
			title:	qsTr("Test Statistic")

			RadioButton { value: "honda";	label: qsTr("Honda"); checked: true	}
			RadioButton { value: "bp";		label: qsTr("Breusch-Pagan")		}
			RadioButton { value: "kw";		label: qsTr("King-Wu")				}

			// only defined for the two-ways case
			RadioButton
			{
				value:		"ghm"
				label:		qsTr("Gourieroux-Holly-Monfort")
				enabled:	lmTestEffects.value === "twoways"
			}
		}
	}
}
