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

		// plm provides no robust covariance matrix for the between estimator
		Common.Coefficients { allowRobust: false }

		// the between estimator averages over one dimension, so there is no
		// two-ways variant
		Common.Effects { allowTwoways: false }

		Common.Plot{}
	}
}
