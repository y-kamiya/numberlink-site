import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class FieldInnerCellComponent extends React.Component {

    static get CLASS_NAME_MAP()  { 
        let map = {}
        map[Store.KEYCODE.LEFT]  = 'bottom';
        map[Store.KEYCODE.UP]    = 'left';
        map[Store.KEYCODE.RIGHT] = 'top';
        map[Store.KEYCODE.DOWN]  = 'right';
        return map;
    };

    constructor(props) {
        super(props);
        this.state = {
            moveFrom: Store.getMoveFrom(this.props.cellId),
            moveTo: Store.getMoveTo(this.props.cellId),
        };
    }

    render() {
        let className = "";
        if (this.shouldDraw()) {
            className = FieldInnerCellComponent.CLASS_NAME_MAP[this.props.inCharge];
        }
        return <td className={className}></td>;
    }

    shouldDraw() {
        let inCharge = this.props.inCharge;
        return inCharge === this.state.moveFrom || inCharge === this.state.moveTo;
    }

    componentDidMount() {
        Store.addEventListener(Store.CHANGE_ACTIVE_CELL, () => {
            this.setState({moveFrom: Store.getMoveFrom(this.props.cellId)});
            this.setState({moveTo: Store.getMoveTo(this.props.cellId)});
        });
    }
}



