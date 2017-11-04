import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'
import FieldCellComponent from '../components/FieldCellComponent'
import FieldInputCellComponent from '../components/FieldInputCellComponent'

export default class FieldComponent extends React.Component {
    constructor() {
        super();
        this.state = {
            appState : Store.getAppState(),
            fieldSize: Store.getFieldSize(),
            cells    : Store.getCells(),
        };
    }

    groupBy(array, n) {
        if (array.length === 0) {
            return [];
        }
        let group = [];
        for (let i = 0; i < n; i++) {
            group.push(array.shift());

        }
        return [group].concat(this.groupBy(array, n));
    }

    render() {
        return (
            <div id="field"><table>{this.createTable()}</table></div>
        );
    }

    createTable(callback) {
        let rowNum = this.state.fieldSize.rowNum;
        let colNum = this.state.fieldSize.colNum;
        let ids = [];
        for (let i = 0; i < rowNum * colNum; i++) {
            ids[i] = i;
        }
        let idGroups = this.groupBy(ids, colNum);
        let table = idGroups.map((innerIds, rowId) => {
            let row = innerIds.map(id => {
                if (this.state.appState === Store.STATE_INITIAL) {
                    return <FieldInputCellComponent key={"cell"+id} cellId={id} />
                } else {
                    return <FieldCellComponent key={"cell"+id} cellId={id} />
                }
            });
            let key = "row" + rowId;
            return <tr key={key}>{row}</tr>;
        });
        return <tbody>{table}</tbody>;
    }

    componentDidMount() {
        Store.addChangeFieldSizeListener(() => {
            this.setState(Store.getFieldSize());
        });
        Store.addEventListener(Store.CREATE_FIELD, () => {
            this.setState({appState: Store.getAppState()});
        });
    }
}


