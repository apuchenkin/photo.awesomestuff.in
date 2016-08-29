import React from 'react';
import CategoryService from '../../service/Category';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import CategoryLink from '../link/category';
import config from '../../config.json';
import {FormattedMessage} from 'react-intl';
import './style.less';

class Home extends React.Component {
	render() {
    let
			props = this.props,
			categories = props.categories,
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
										<CategoryLink category={category.parent ? category.parent.name : category.name} className="active">
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
			<div className="galleries">
				<h2>
					<FormattedMessage
							id="galleries"
							defaultMessage={`Galleries`}
					/>
				</h2>
				<ul>{galleries}</ul>
			</div>
		);
	}
}

Home.propTypes = {
  categories: React.PropTypes.array.isRequired
};

export default Home;
