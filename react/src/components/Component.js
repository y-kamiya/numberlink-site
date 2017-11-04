import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'
import FieldSizeComponent from '../components/FieldSizeComponent'
import FieldComponent from '../components/FieldComponent'
import ButtonComponent from '../components/ButtonComponent'

export default class Component extends React.Component {
    constructor() {
        super();
    }

    componentDidMount() {
        window.addEventListener('keydown', this.onKeyDown.bind(this));
    }

    componentiWillUnmount() {
        window.removeEventListener('keydown', this.onKeyDown.bind(this));
    }

    render() {
        return (
            <div>
                <FieldSizeComponent />
                <FieldComponent />
                <ButtonComponent />
            </div>
        );
    }

    onKeyDown(e) {
        if (!this.isValidKey(e.keyCode)) {
            return;
        }
        e.preventDefault();
        AppActions.moveCursor(e.keyCode);
    }

    isValidKey(keycode) {
        let keys = Object.keys(Store.KEYCODE);
        return keys.some(function (key) {
            return Store.KEYCODE[key] == keycode;
        });
    }

    
}
