import React from 'react';
import Loader from '../loader';

class Page extends React.Component {
	render() {
		return (
      <div>
  			<div className="page" dangerouslySetInnerHTML={{__html: this.props.content}} ></div>
      </div>
		);
	}
}

Page.propTypes = {
  content: React.PropTypes.string.isRequired
};

export default Page;
