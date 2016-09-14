import React from 'react';
import { bind } from 'decko';

const { bool, shape, func, element } = React.PropTypes;

export default class LoadingContext extends React.Component {

  static propTypes = {
    history: shape({
      listen: func.isRequired,
    }),
    children: element.isRequired,
  }

  static childContextTypes = {
    isLoading: bool,
  };

  constructor(props) {
    super(props);

    this.unlisten = props.history.listen(() => this.startLoading());
    this.state = {
      isLoading: false,
    };
  }

  getChildContext() {
    return { isLoading: this.state.isLoading };
  }

  componentWillUnmount() {
    this.unlisten();
  }

  @bind
  startLoading() {
    console.log('startLoading');
    this.setState({ isLoading: true });
  }

  @bind
  stopLoading() {
    console.log('stopLoading');
    this.setState({ isLoading: false });
  }

  render() {
    const
      { children } = this.props;

    return React.cloneElement(children, {
      startLoading: this.startLoading,
      stopLoading: this.stopLoading,
    });
  }
}
