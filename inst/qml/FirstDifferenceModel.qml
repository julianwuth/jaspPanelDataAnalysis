import QtQuick
import JASP.Controls
import JASP

import './Common' as Common

// plm only defines the first-difference estimator for individual effects, so no
// effect selector is offered.
Form
{
	Common.VariableInput{}

	Section
	{
		title: qsTr("Statistics")

		Common.Coefficients{}
		Common.Plot{}
	}
}
