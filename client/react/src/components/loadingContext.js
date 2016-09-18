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
    startLoading: func,
    stopLoading: func,
  };

  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
    };
  }

  @bind
  getChildContext() {
    return {
      isLoading: this.state.isLoading,
      startLoading: this.startLoading,
      stopLoading: this.stopLoading,
    };
  }

  componentDidMount() {
    this.unlisten = this.props.history.listen(() => this.startLoading());
  }

  componentWillUnmount() {
    this.unlisten();
  }

  @bind
  startLoading() {
    this.setState({ isLoading: true });
  }

  @bind
  stopLoading() {
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
