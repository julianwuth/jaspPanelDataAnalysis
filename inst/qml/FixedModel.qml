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

        CheckBox
        {
            name: "fixedEffects"
            label: qsTr("Fixed effect estimates")
            checked: false
        }

        Common.Effects{}
        Common.Plot{}
    }
}