import React from 'react';
// import PhotoService from '../../service/Photo';
// import './style.less';

class Gallery extends React.Component {

	constructor(props, context) {
    super(props, context);

    this.state = {
      category: this.props.params.category,
      subcategory: this.props.params.subcategory,
      categories: context.initialState.categories || []
    }
  }

	render() {
		return (
			<div>
				<h1>Gallery: {this.state.category}/{this.state.subcategory}</h1>
			</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
