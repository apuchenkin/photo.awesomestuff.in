import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

const { string } = React.PropTypes;

export default class Page extends React.Component {

  static propTypes = {
    content: string.isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      <div className="page" dangerouslySetInnerHTML={{ __html: this.props.content }} />
    );
  }
}
