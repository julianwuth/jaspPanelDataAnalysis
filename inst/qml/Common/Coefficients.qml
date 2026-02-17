import QtQuick
import JASP.Controls
import JASP


Group
{
    title: qsTr("Coefficients")

    CheckBox
    {
        id: step1
        name: "estimates"
        label: qsTr("Estimates")

    // We can add some extra control parameters
        checked: false // Default value

        CheckBox
        {
            id: step2
            name: "robust_estimates"
            label: qsTr("Robust Estimates")

            enabled: step1.checked

            onEnabledChanged: {
                if (!enabled) checked = false
            }
        }
    }
}
