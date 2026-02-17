import QtQuick
import JASP.Controls
import JASP

Group
{
    VariablesForm
    {
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList  {
        name: "dependent"
        label: qsTr("Dependent variables")
        info: qsTr("")
        singleVariable: false
        allowedColumns: ["scale", "ordinal", "nominal"]
        }

        AssignedVariablesList  {
        name: "covariates"
        label: qsTr("Covariates")
        info: qsTr("")
        singleVariable: false
        allowedColumns: ["scale"]
        }

        AssignedVariablesList  {
        name: "factors"
        label: qsTr("Factors")
        info: qsTr("")
        singleVariable: false
        allowedColumns: ["ordinal", "nominal"]
        }

        AssignedVariablesList  {
        name: "id"
        label: qsTr("ID")
        info: qsTr("")
        singleVariable: true
        allowedColumns: ["nominal"]
        }
        
        AssignedVariablesList  {
            name: "time"
            label: qsTr("Time")
            info: qsTr("")
            singleVariable: true
            allowedColumns: ["ordinal"]
            enabled: !idOnly.checked
            onEnabledChanged: if (!enabled && count > 0) itemDoubleClicked(0);
        }
    }

    CheckBox
    {
        name: "idOnly"
        id: idOnly
        label: qsTr("ID only")
        checked: false
        info: qsTr("If checked, only the ID variable needs to be specified. The time variable will be automatically created based on the assumption that the data is ordered correctly.")
    } 
}