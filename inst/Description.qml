import QtQuick
import JASP.Module

Description
{
	name		: "jaspPanelData"
	title		: qsTr("Panel Data")
	description	: qsTr("Module for analyzing panel data")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon        : "exampleIcon.png" // Located in /inst/icons/
	preloadData: true
	requiresData: true


	GroupTitle
	{
		title: qsTr("Basic Models")
	}

	Analysis
	{
		title: qsTr("Panel data analysis")
		func: "panelDataAnalysis"
		qml: "panelDataAnalysis.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Fixed Effects Model")
		func: "fixedModel"
		qml: "FixedModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Random Effects Model")
		func: "randomModel"
		qml: "RandomModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Hausman-Taylor Model")
		func: "htModel"
		qml: "HTModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Pooling Model")
		func: "poolingModel"
		qml: "PoolingModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("First-Difference Model")
		func: "firstDifferenceModel"
		qml: "FirstDifferenceModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Between Model")
		func: "betweenModel"
		qml: "BetweenModel.qml"
		requiresData: false
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Advanced Models")
	}

	Analysis
	{
		title: qsTr("Variable Coefficients Models")
		func: "vcmModel"
		qml: "VCMModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Generalized Method of Moments")
		func: "gmmModel"
		qml: "GMMModel.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("General Feasible Generalized Least Squares")
		func: "pgglsModel"
		qml: "PGGLSModel.qml"
		requiresData: false
	}
}
