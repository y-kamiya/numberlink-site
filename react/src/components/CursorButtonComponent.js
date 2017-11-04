import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class CursorButtonComponent extends React.Component {
    constructor() {
        super();
        this.state = {
            appState: Store.getAppState(),
        };
    }

    componentDidMount() {
        Store.addEventListener(Store.CREATE_FIELD, () => {
            this.setState({appState: Store.getAppState()});
        });
        document.getElementById('cursor-up').addEventListener('click', this.onClick(Store.KEYCODE.UP), false);
        document.getElementById('cursor-left').addEventListener('click', this.onClick(Store.KEYCODE.LEFT), false);
        document.getElementById('cursor-right').addEventListener('click', this.onClick(Store.KEYCODE.RIGHT), false);
        document.getElementById('cursor-down').addEventListener('click', this.onClick(Store.KEYCODE.DOWN), false);
    }

    onClick(keycode) {
        return () => AppActions.moveCursor(keycode);
    };

    render() {
        let className = this.isDisabled() ? "disabled" : "";
        return (
            <div className={"cursor " + className}>
                <div>
                    <span id="cursor-up" className="arrow-primary btn btn-lg"></span>
                </div>
                <div>
                    <span id="cursor-left" className="arrow-primary btn btn-lg" data-angle="270"></span>
                    <span id="cursor-right" className="arrow-primary btn btn-lg" data-angle="90"></span>
                </div>
                <div>
                    <span id="cursor-down" className="arrow-primary btn btn-lg" data-angle="180"></span>
                </div>
            </div>
        );
    }

    isDisabled() {
        if (this.state.appState === Store.STATE_INITIAL) {
            return true;
        }
        return false;
    }

}


