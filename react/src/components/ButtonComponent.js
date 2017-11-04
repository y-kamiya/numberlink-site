import React from 'react'
import Store from '../stores/Store'
import AppActions from '../actions/AppActions'
import CreateTableButtonComponent from '../components/CreateTableButtonComponent'
import SolveButtonComponent from '../components/SolveButtonComponent'
import CursorButtonComponent from '../components/CursorButtonComponent'

export default class ButtonsComponent extends React.Component {
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
            <div id="buttons">
                <CreateTableButtonComponent />
                <SolveButtonComponent />
                <CursorButtonComponent />
            </div>
        );
    }
}

