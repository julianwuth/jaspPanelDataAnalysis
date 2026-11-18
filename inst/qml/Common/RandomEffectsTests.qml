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
		info:		qsTr("Tests whether the variance of the unobserved effect is zero, assuming homoskedastic and serially uncorrelated errors. A rejection can also reflect serial correlation or heteroskedasticity, so it does not establish a random effects structure.")

		RadioButtonGroup
		{
			id:		lmTestEffects
			name:	"lmTestEffects"
			title:	qsTr("Effects")
			info:	qsTr("The unobserved effects that are tested.")

			RadioButton { value: "individual";	label: qsTr("Individual"); checked: true	}
			RadioButton { value: "time";		label: qsTr("Time")							}
			RadioButton { value: "twoways";		label: qsTr("Individual and Time")			}
		}

		RadioButtonGroup
		{
			name:	"lmTestType"
			title:	qsTr("Test Statistic")
			info:	qsTr("The form of the statistic. Honda and King-Wu are one-sided; the Gourieroux-Holly-Monfort statistic requires individual and time effects.")

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
