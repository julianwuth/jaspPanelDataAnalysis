import QtQuick
import JASP.Controls
import JASP

Group
{
    title: qsTr("Plot")

    CheckBox
    {
        name : "plot"
        label : qsTr("Plot")
        checked : false
        info : qsTr("Plot the dependent variable over time, with one line per individual. The legend is omitted when there are more than 20 individuals.")
    }
}
