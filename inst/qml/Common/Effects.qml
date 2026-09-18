import QtQuick
import JASP.Controls
import JASP

// Which effects an estimator accepts differs per model, so the buttons that plm
// rejects are hidden rather than offered and then failing.
RadioButtonGroup
{
	name:	"effects"
	title:	qsTr("Effects")
	info:	qsTr("The dimension along which the panel is transformed.")

	property bool allowTime:	true
	property bool allowTwoways:	true

	RadioButton { value: "individual";	label: qsTr("Individual");			checked: true			}
	RadioButton { value: "time";		label: qsTr("Time");				visible: allowTime		}
	RadioButton { value: "twoways";		label: qsTr("Individual and Time");	visible: allowTwoways	}
}
