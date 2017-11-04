import AppDispatcher from '../dispatcher/AppDispatcher'

class AppActions {
    get CHANGE_ROWNUM_ACTION() { return 'change_rownum_action' };
    get CHANGE_COLNUM_ACTION() { return 'change_colnum_action' };
    get CREATE_FIELD_ACTION() { return 'create_field_action' };
    get CHANGE_INPUT_CELL() { return 'change_input_cell_action' };
    get CHANGE_ACTIVE_CELL() { return 'change_active_cell_action' };
    get MOVE_CURSOR() { return 'move_cursor' };

    changeRowNum(rowNum) {
        AppDispatcher.dispatch({
            actionType: this.CHANGE_ROWNUM_ACTION,
            value: rowNum
        });
    }

    changeColNum(colNum) {
        AppDispatcher.dispatch({
            actionType: this.CHANGE_COLNUM_ACTION,
            value: colNum
        });
    }

    createField() {
        AppDispatcher.dispatch({
            actionType: this.CREATE_FIELD_ACTION,
        });
    }

    changeInputCell(cellId, value) {
        AppDispatcher.dispatch({
            actionType: this.CHANGE_INPUT_CELL,
            cellId: cellId,
            value: value,
        });
    }

    changeActiveCell(cellId) {
        AppDispatcher.dispatch({
            actionType: this.CHANGE_ACTIVE_CELL,
            cellId: cellId,
        });
    }

    moveCursor(keycode) {
        AppDispatcher.dispatch({
            actionType: this.MOVE_CURSOR,
            keycode: keycode,
        });
    }

}

module.exports = new AppActions();
