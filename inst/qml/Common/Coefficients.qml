import QtQuick
import JASP.Controls
import JASP

Group
{
	title: qsTr("Coefficients")

	// plm only provides robust covariance matrices for the within, random,
	// pooling and first-difference estimators
	property bool allowRobust: true

	CheckBox
	{
		id:			estimates
		name:		"estimates"
		label:		qsTr("Estimates")
		checked:	false
		info:		qsTr("Display the estimated regression coefficients with their standard errors and tests.")

		CheckBox
		{
			name:		"robustEstimates"
			label:		qsTr("Robust standard errors")
			checked:	false
			visible:	allowRobust
			enabled:	estimates.checked
			info:		qsTr("Use heteroskedasticity and autocorrelation consistent (Arellano) standard errors clustered on the individuals.")

			onEnabledChanged: { if (!enabled) checked = false }
		}
	}
}
