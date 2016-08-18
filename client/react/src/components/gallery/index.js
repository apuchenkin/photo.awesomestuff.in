import React from 'react';
import Category from '../link/category';
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
    let
      state = this.state,
      category = state.categories.find(c => c.name == state.category),
      subcategory = state.subcategory && state.categories.find(c => c.name == state.subcategory),
      childs = state.categories.filter(c => c.parent && c.parent.name === state.category).map(category => {
          return (
            <li className="item" key={category.id} >
              <Category data={category} />
            </li>
          );
    });

		return (
			<div>
				<h1>Gallery: <Category data={category} />/{subcategory && <Category data={subcategory} />}</h1>
        <nav>{childs}</nav>
			</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
