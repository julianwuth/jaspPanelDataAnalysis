import QtQuick
import JASP.Module

Description
{
	name		: "jaspModuleTemplate"
	title		: qsTr("Panel Data Analysis")
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
		title:	qsTr("Basic interactivity")
	}

	Analysis
	{
		title: qsTr("Using the interface") // Title for window
		menu: qsTr("Using the interface")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: false                // Allow to run even without data
	}

	Analysis
	{
	  title: qsTr("Loading data")
	  menu: qsTr("Loading data")
	  func: "processTable"
	  qml: "LoadingData.qml"
	}

	GroupTitle
	{
		title:	qsTr("Basic functions")
	}

	Analysis
	{
	  title: qsTr("Add one")        // Title for window
	  menu: qsTr("Add one")         // Title for ribbon
	  func: "addOne"                // Function to be called
    qml: "AddOne.qml"            // Design input window
	  requiresData: false           // Allow to run even without data
	}

	GroupTitle
	{
	  title: qsTr("Plotting")
	}

	Analysis
	{
	  title: qsTr("Plot a parabola")
	  func: "parabola"
	  qml: "Parabola.qml"
	  requiresData: false
	}

	Separator{}

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


}
