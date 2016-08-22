import React from 'react';
import Link from 'react-router/lib/Link';

import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

var isBrowser = (typeof window !== 'undefined');
var Ps = isBrowser ? window.Ps || require('perfect-scrollbar') : null;

class Main extends React.Component {

    constructor(props, context) {
	    super(props, context);

	    this.state = {
				categories: context.initialState.categories || []
			}
	  }

  	componentDidMount() {
  		const
        me = this,
        props = me.props
      ;

      me.adjustHeader();
      Ps.initialize(me.refs.content);
      if (me.state.categories !== me.context.initialState.categories) {
        props.route.resolve(props.params).categories
    			.then(categories => me.setState({categories: categories}));
      }
  	}

    componentDidUpdate() {
      const content = this.refs.content;
      this.adjustHeader();
      content.scrollTop = 0;
      Ps.update(content);
    }

    adjustHeader() {
      const refs = this.refs;

      refs.main.style.paddingTop = refs.header.refs.main.clientHeight + 'px';
    }

    render() {
      const
        categories = this.state.categories,
        header = categories && categories.length && this.props.header && React.cloneElement(this.props.header, {
          categories: categories,
          ref: "header"
        }),
        childrens = categories && categories.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
          categories: categories
        }));

      return (
        <div id="main" ref="main">
          {header}
          <div className="content" ref="content">
            {this.props.body}
            {childrens}
            <footer>
              <Link to="/">photo.awesomestuff.in</Link> | © 2015, Пученкин Артём | <Link to="/about">О сайте</Link> | <Link to="/contacts">Контакты</Link>
            </footer>
          </div>
        </div>
      );
    }
}

Main.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Main;
