import React from 'react';

class ExtraDataProvider extends React.Component {
  getChildContext() {
    return { initialState: this.initialState }
  }

  constructor(props, context) {
    super(props, context)
    this.initialState = props.initialState
  }

  render() {
    return React.Children.only(this.props.children)
  }
}

ExtraDataProvider.childContextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default ExtraDataProvider;
