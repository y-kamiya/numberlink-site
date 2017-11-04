import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class Component extends React.Component {
    constructor() {
        super();
        this.state = Store.getFieldSize();
        this.state.appState = Store.getAppState();
    }

    render() {
        let className = this.isDisabled() ? "disabled" : "";
        return (
            <div className={"fieldSize " + className} >
                <span>row: <input type="number" id="rowNum" value={this.state.rowNum} onChange={this.onChangeRow.bind(this)}/></span>
                <span>colomn: <input type="number" id="colNum" value={this.state.colNum} onChange={this.onChangeCol.bind(this)}/></span>
            </div>
        );
    }

    onChangeRow(e) {
        AppActions.changeRowNum(e.target.value);
    }

    onChangeCol(e) {
        AppActions.changeColNum(e.target.value);
    }

    componentDidMount() {
        Store.addEventListener(Store.CHANGE_FIELD_SIZE, () => {
            this.setState(Store.getFieldSize());
        });
        Store.addEventListener(Store.CREATE_FIELD, () => {
            this.setState({appState: Store.getAppState()});
        });
    }

    isDisabled() {
        if (this.state.appState === Store.STATE_INITIAL) {
            return false;
        }
        return true;
    }

}

