import EventEmitter from 'events'
import AppActions from '../actions/AppActions'
import AppDispatcher from '../dispatcher/AppDispatcher'

let instance = null;

class Store extends EventEmitter {
    get CHANGE_FIELD_SIZE() { return 'change_field_size' };
    get CREATE_FIELD() { return 'create_field' };
    get CHANGE_INPUT_CELL() { return 'change_input_cell' };
    get CHANGE_ACTIVE_CELL() { return 'change_active_cell' };

    get STATE_INITIAL() { return 0 };
    get STATE_FIELD_CREATED() { return 1 };

    get NO_CELL() { return -1 };
    get KEYCODE() { return {LEFT: 37, UP: 38, RIGHT: 39, DOWN: 40} };
    get COUNTER_DIRECTION_MAP()  { 
        let map = {};
        map[this.KEYCODE.LEFT]  = this.KEYCODE.RIGHT;
        map[this.KEYCODE.RIGHT] = this.KEYCODE.LEFT;
        map[this.KEYCODE.UP]    = this.KEYCODE.DOWN;
        map[this.KEYCODE.DOWN]  = this.KEYCODE.UP;
        return map;
    }

    constructor() {
        if (instance) {
            return instance;
        }
        super();
        this.fieldSize = {
            rowNum: 5,
            colNum: 5,
        };
        this.appState = this.STATE_INITIAL;
        this.cells = [];
        this.moveFrom = [];
        this.moveTo = [];
        this.activeCell = this.NO_CELL;
        AppDispatcher.register(this.onAction.bind(this));
        this.setMaxListeners(1000);

        instance = this;
    }

    getFieldSize() {
        return this.fieldSize;
    }

    getAppState() {
        return this.appState;
    }

    getCells() {
        return this.cells;
    }

    getCellValue(id) {
        return this.cells[id];
    }

    getMoveFrom(id) {
        return this.moveFrom[id];
    }

    getMoveTo(id) {
        return this.moveTo[id];
    }

    getActiveCell() {
        return this.activeCell;
    }

    emitChangeFieldSize() {
        this.emit(this.CHANGE_FIELD_SIZE);
    }

    emitNewAppState() {
        this.emit(this.CREATE_FIELD);
    }

    addEventListener(event, callback) {
        this.on(event, callback);
    }
    addChangeFieldSizeListener(callback) {
        this.on(this.CHANGE_FIELD_SIZE, callback);
    }

    onAction(payload) {
        switch (payload.actionType) {
            case AppActions.CHANGE_ROWNUM_ACTION:
                this.fieldSize.rowNum = payload.value;
                this.emitChangeFieldSize();
                break;
            case AppActions.CHANGE_COLNUM_ACTION:
                this.fieldSize.colNum = payload.value;
                this.emitChangeFieldSize();
                break;
            case AppActions.CREATE_FIELD_ACTION:
                this.appState = this.STATE_FIELD_CREATED;
                this.emitNewAppState();
                break;
            case AppActions.CHANGE_INPUT_CELL:
                this.cells[payload.cellId] = payload.value;
                this.emit(this.CHANGE_INPUT_CELL);
                break;
            case AppActions.CHANGE_ACTIVE_CELL:
                this.activeCell = payload.cellId;
                this.emit(this.CHANGE_ACTIVE_CELL);
                break;
            case AppActions.MOVE_CURSOR:
                let keycode = payload.keycode;
                let oldCell = this.activeCell;
                this.activeCell = this.getNextActiveCell(keycode);
                if (oldCell === this.activeCell) {
                    break;
                }
                this.moveTo[oldCell] = keycode;
                this.moveFrom[this.activeCell] = this.getCounterDirection(keycode);
                this.emit(this.CHANGE_ACTIVE_CELL);
                break;
        }
    }

    getCounterDirection(keycode) {
        return this.COUNTER_DIRECTION_MAP[keycode];
    }

    getNextActiveCell(keycode) {
        let id = this.activeCell;
        if (id === this.NO_CELL) {
            return this.NO_CELL;
        }

        let colNum = this.fieldSize.colNum;
        let row = id / colNum;
        let col = id % colNum;
        switch (keycode) {
            case this.KEYCODE.LEFT:
                if (col - 1 < 0) return this.activeCell;
                return id - 1;
            case this.KEYCODE.UP:
                if (row - 1 < 0) return this.activeCell;
                return id - colNum;
            case this.KEYCODE.RIGHT:
                if (colNum <= col + 1) return this.activeCell;
                return id + 1;
            case this.KEYCODE.DOWN:
                if (this.fieldSize.rowNum <= row + 1) return this.activeCell;
                return id + colNum;
            default:
                return this.activeCell;
        }

    }
}

module.exports = new Store();
