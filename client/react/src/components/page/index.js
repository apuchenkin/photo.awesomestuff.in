import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import Loader from '../loader';

const { string } = React.PropTypes;

export default class Page extends React.Component {

  static propTypes = {
    content: string.isRequired
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      <div>
        <div className="page" dangerouslySetInnerHTML={{__html: this.props.content}} ></div>
      </div>
    );
  }
}
