import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class CreateTableButtonComponent extends React.Component {
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
    }

    componentiWillUnmount() {
    }

    onClick() {
        AppActions.createField();
    }

    render() {
        let className = this.isDisabled() ? "disabled" : "";
        return (
            <div className={className}>
                <button id="createTableButton" className="btn btn-lg btn-primary" onClick={this.onClick.bind(this)}>Create Field</button>
            </div>
        );
    }

    isDisabled() {
        if (this.state.appState === Store.STATE_INITIAL) {
            return false;
        }
        return true;
    }
}


