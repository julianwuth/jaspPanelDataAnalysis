import JASP.Controls
import JASP
import './Common' as Common

Form
{
    Text { text: qsTr("Effects") }
    Common.VariableInput{}

    Section
    {
        title : qsTr("Statistics")

        Text { text: qsTr("Effects") }

        RadioButtonGroup
		{
			name: "a"
			title: qsTr("Effects")

			RadioButton { value: "fixed"; label: qsTr("Fixed"); checked: true } // Single-line definition is also possible
			RadioButton { value: "random"; label: qsTr("Random") }
		}
    }

}
