import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class FieldInputCellComponent extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            cellValue: Store.getCellValue(this.props.cellId),
        };
    }

    render() {
        let id = this.props.cellId;
        return (
            <td id={id}>
                <input type="number"
                       onChange={this.onChangeCell}
                       value={this.state.cellValue} />

            </td>
        );
    }

    onChangeCell(e) {
        let input = e.target;
        let td = input.parentNode;
        AppActions.changeInputCell(td.id, input.value);
    }

}



