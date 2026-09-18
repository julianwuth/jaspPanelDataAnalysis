import QtQuick
import JASP.Controls
import JASP

Section
{
	title: qsTr("Assumption Checks")

	CheckBox
	{
		name:		"hausmanTest"
		label:		qsTr("Hausman test")
		checked:	false
		info:		qsTr("Compares the fixed and the random effects estimator; a significant result favours the fixed effects model.")
	}
}
