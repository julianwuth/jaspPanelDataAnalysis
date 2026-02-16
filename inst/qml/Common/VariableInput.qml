//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
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