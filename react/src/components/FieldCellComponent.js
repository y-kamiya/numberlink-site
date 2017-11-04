import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'
import FieldInnerCellComponent from '../components/FieldInnerCellComponent'

export default class FieldCellComponent extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            cellValue: Store.getCellValue(this.props.cellId),
            activeCell: Store.getActiveCell(),
        };
    }

    render() {
        let className = this.isActive() ? 'current' : '';
        let cellValue = this.state.cellValue; 
        let contents = 0 <= cellValue ? cellValue : this.buildInnerCell();
        return <td id={this.props.cellId} className={className} onClick={this.onClick.bind(this)}>
            {contents}
        </td>;
    }

    buildInnerCell() {
        let Cell = FieldInnerCellComponent;
        return (
            <table className="innerTable"><tbody>
                <tr>
                    <Cell cellId={this.props.cellId} inCharge={Store.KEYCODE.LEFT}/>
                    <Cell cellId={this.props.cellId} inCharge={Store.KEYCODE.UP}/>
                </tr>
                <tr>
                    <Cell cellId={this.props.cellId} inCharge={Store.KEYCODE.DOWN}/>
                    <Cell cellId={this.props.cellId} inCharge={Store.KEYCODE.RIGHT}/>
                </tr>
            </tbody></table>
        );
    }
    onClick() {
        AppActions.changeActiveCell(this.props.cellId);
    }

    componentDidMount() {
        Store.addEventListener(Store.CHANGE_ACTIVE_CELL, () => {
            this.setState({activeCell: Store.getActiveCell()});
        });
    }

    isActive() {
        return this.state.activeCell === this.props.cellId;
    }

}




