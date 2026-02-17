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
        RadioButtonGroup
        {
            name : 'estimators'
            title : qsTr("Estimators")
            RadioButton { value: "swar"; label: qsTr("Swamy-Aurora"); checked: true }
            RadioButton { value: "walhus"; label: qsTr("Wallace-Hussain") }
            RadioButton { value: "anemiya"; label: qsTr("Amemiya") }
            RadioButton { value: "nerlove"; label: qsTr("Nerlove") }
        }
    }
}
