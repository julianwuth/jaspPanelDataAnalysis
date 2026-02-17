import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets
import JASP

import './Common' as Common

Form{
    Common.VariableInput{}
    Section
    {
        title : qsTr("Statistics")
        Common.Coefficients{}
        Common.Plot{}

        Group
        {
            title : qsTr("Tests")

            CheckBox
            {
                name: "lmt"
                label: qsTr("Lagrange Multiplier Test")


                RadioButtonGroup
                {
                    name: "effects"
                    title: qsTr("Effects")

                    RadioButton { value: "individual"; label: qsTr("Individual"); checked: true }
                    RadioButton { value: "time"; label: qsTr("Time") }
                    RadioButton { value: "twoways"; label: qsTr("Individual and Time") }
                }

                RadioButtonGroup
                {
                    name: "type"
                    title: qsTr("Test Statistic")

                    RadioButton { value: "honda"; label: qsTr("Honda"); checked: true }
                    RadioButton { value: "bp"; label: qsTr("Breusch-Pagan") }
                    RadioButton { value: "kw"; label: qsTr("King-Wu") }
                    RadioButton { value: "ghm"; label: qsTr("Gourieroux-Holly-Monfort ") }
                }
            }
        }
    }
}