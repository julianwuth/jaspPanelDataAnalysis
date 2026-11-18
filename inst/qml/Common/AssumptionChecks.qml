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
		info:		qsTr("Compares the fixed and the random effects estimator to test whether the individual effects are uncorrelated with the regressors. Both estimators require strict exogeneity. With robust standard errors a cluster-robust form of the test is used; the classical form also assumes homoskedastic, serially uncorrelated errors.")
	}
}
