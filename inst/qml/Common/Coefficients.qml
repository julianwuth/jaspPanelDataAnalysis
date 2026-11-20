import QtQuick
import JASP.Controls
import JASP

Group
{
	title: qsTr("Coefficients")

	// valid robust covariance matrices are only available for the within, random,
	// pooling and first-difference estimators
	property bool allowRobust: true

	CheckBox
	{
		name:		"estimates"
		label:		qsTr("Estimates")
		checked:	false
		info:		qsTr("Display the estimated regression coefficients with their standard errors and tests.")
	}

	// also drives the overall and Hausman tests, so it does not require the estimates table
	CheckBox
	{
		name:		"robustEstimates"
		label:		qsTr("Robust standard errors")
		checked:	false
		visible:	allowRobust
		info:		qsTr("Use heteroskedasticity and autocorrelation consistent (Arellano) standard errors clustered on the individuals, in the coefficients table, the overall test and the Hausman test.")
	}
}
