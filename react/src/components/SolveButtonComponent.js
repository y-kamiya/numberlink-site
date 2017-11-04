import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'

export default class SolveButtonComponent extends React.Component {
    constructor() {
        super();
        this.state = {
            appState: Store.getAppState(),
        };
    }

    componentDidMount() {
    }

    componentiWillUnmount() {
    }

    render() {
        return (
            <div className="disabled">
                <button id="solve" className="btn btn-lg btn-primary">solve</button>
            </div>
        );
    }
}


