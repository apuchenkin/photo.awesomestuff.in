import React from 'react';
import Loader from '../loader';

const { string } = React.PropTypes;

export default class Page extends React.Component {

  static propTypes = {
    content: string.isRequired
  }

  render() {
    return (
      <div>
        <div className="page" dangerouslySetInnerHTML={{__html: this.props.content}} ></div>
      </div>
    );
  }
}
