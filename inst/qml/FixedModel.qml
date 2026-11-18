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
			name:		"fixedEffects"
			label:		qsTr("Fixed effect estimates")
			checked:	false
			info:		qsTr("Display the estimated individual (and/or time) intercepts.")

			DropDown
			{
				name:	"fixedEffectsType"
				label:	qsTr("Type")
				info:	qsTr("Report the effects as levels, as differences from the first category, or as deviations from their observation-weighted mean.")
				values:
				[
					{ label: qsTr("Level"),					value: "level"	},
					{ label: qsTr("Deviation from first"),	value: "dfirst"	},
					{ label: qsTr("Deviation from mean"),	value: "dmean"	}
				]
			}
		}

		Common.Effects{}
		Common.Plot{}
	}

	Common.AssumptionChecks{}
}
