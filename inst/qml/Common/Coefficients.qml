import QtQuick
import JASP.Controls
import JASP


Group
{
    title: qsTr("Coefficients")

    CheckBox
    {
        name: "estimates"
        label: qsTr("Estimates")

    // We can add some extra control parameters
        checked: false // Default value
    }

    CheckBox
    {
        name: "robust_estimates"
        label: qsTr("Robust Estimates")

    // We can add some extra control parameters
        checked: false // Default value
    }
}
