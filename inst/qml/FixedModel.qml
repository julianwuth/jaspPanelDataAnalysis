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
    }
}