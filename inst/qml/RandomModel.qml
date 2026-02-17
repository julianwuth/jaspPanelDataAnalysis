import JASP.Controls
import JASP
import './Common' as Common

Form
{
    Common.VariableInput{}
    Section
    {
        title : qsTr("Statistics")
        Common.Coefficients{}
        Common.Effects{}
        
        CheckBox
        {
            name: "randomEffects"
            id: randomEffects
            label: qsTr("Random effect estimates")
            checked: false

            RadioButtonGroup
            {
                name : 'estimators'
                title : qsTr("Estimators")
                enabled: randomEffects.checked
                RadioButton { value: "swar"; label: qsTr("Swamy-Aurora"); checked: true }
                RadioButton { value: "walhus"; label: qsTr("Wallace-Hussain") }
                RadioButton { value: "amemiya"; label: qsTr("Amemiya") }
                RadioButton { value: "nerlove"; label: qsTr("Nerlove") }
            }
        }
        
        Common.Plot{}
    }
}
