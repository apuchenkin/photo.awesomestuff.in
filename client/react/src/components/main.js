import React from 'react';
import Link from 'react-router/lib/Link';
import Loader from './loader';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import {FormattedMessage} from 'react-intl';

import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

var isBrowser = (typeof window !== 'undefined');
var Ps = isBrowser ? window.Ps || require('perfect-scrollbar') : null;

class Main extends React.Component {
    constructor (props) {
      super(props)

      this.state = {
        isLoading: false,
        class: props.routes[props.routes.length - 1].class
      }
    }

  	componentDidMount() {
  		const
        me = this,
        props = me.props
      ;

      props.route.cmp = this;
      Ps.initialize(me.refs.content);
  	}

    componentDidUpdate() {
      const content = this.refs.content;
      content.scrollTop = 0;
      Ps.update(content);
    }

    componentWillReceiveProps(props) {
      this.setState({
        class: props.routes[props.routes.length - 1].class
      })
    }


    render() {
      const
        state = this.state,
        props = this.props,
        aboutPage = props.pages.find(p => p.alias == 'about'),
        contactsPage = props.pages.find(p => p.alias == 'contacts')
      ;

      return (
        <div id="main" className={state.class} ref="main">
          <ReactCSSTransitionGroup transitionName="loader" transitionAppearTimeout={200} transitionEnterTimeout={200} transitionLeaveTimeout={200} transitionAppear={false}>
            {this.state.isLoading && <Loader />}
          </ReactCSSTransitionGroup>
          {this.props.header}
          <div className="content" ref="content">
            {this.props.body}
            <footer>
              <Link to="/">photo.awesomestuff.in</Link> | &copy; <FormattedMessage
                    id="footer"
                    defaultMessage={`2016, Artem Puchenkin`}
                />
              {aboutPage && [" | ", <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
              {contactsPage && [" | ", <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
            </footer>
          </div>
        </div>
      );
    }
}

Main.propTypes = {
  pages: React.PropTypes.array.isRequired
};

export default Main;
