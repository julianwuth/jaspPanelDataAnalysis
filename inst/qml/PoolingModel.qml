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
    }
}