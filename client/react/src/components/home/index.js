import React from 'react';
import CategoryService from '../../service/Category';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import CategoryLink from '../link/category';
import config from '../../config.json';
import './style.less';

class Home extends React.Component {
	constructor(props, context) {
    let
      params = props.params,
      initial = context.initialState;

    super(props, context);

    this.state = {
			categories: initial.categories || props.categories || [],
			showHidden: false
		}
  }

	render() {
    let
			state = this.state,
			categories = state.categories,
      galleries = categories.filter(c => !c.parent && c.title && c.image).map(function(category) {
          return (
            <li key={category.id} >
							<div className="gallery">
								<CategoryLink category={category.parent ? category.parent.name : category.name} className="cover">
									<img src={config.apiEndpoint + config.apiPrefix + '/' + category.image} width={config.gallery.width} title={category.title} alt={category.title} />
									{category.date && <span className="sub">{category.date}</span>}
								</CategoryLink>
								<aside>
									<h3>
										<CategoryLink category={category.parent ? category.parent.name : category.name}>
										{category.title}
										</CategoryLink>
									</h3>
									{category.childs && !!category.childs.length && <ul>
										{category.childs
											.map(cid => categories.find(c => c.id == cid))
											.map(c => (<li key={c.id}><CategoryLink category={c.parent ? c.parent.name : c.name} subcategory={c.parent && c.name}>{c.title}</CategoryLink></li>))}
									</ul>}
								</aside>
							</div>
            </li>
          );
    });

		return (
			<div className="content">
        <div className="galleries">
					<h2>Галереи</h2>
          <ul>{galleries}</ul>
        </div>
			</div>
		);
	}
}

Home.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Home;
