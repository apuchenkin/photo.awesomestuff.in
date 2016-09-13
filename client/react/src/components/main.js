import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import Animate from 'rc-animate';
import { FormattedMessage } from 'react-intl';
import { locationShape } from 'react-router/lib/PropTypes';
import withRouter from 'react-router/lib/withRouter';

import Langs from './common/langs';
import Link from './link';
import Loader from './loader';
import config from '../config/config.json';

import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

const {bool, array, string, element} = React.PropTypes;
const isBrowser = (typeof window !== 'undefined');
const Ps = isBrowser ? window.Ps || require('perfect-scrollbar') : null;

class Main extends React.Component {

  static childContextTypes = {
    prefix: string
  }

  static contextTypes = {
    isLoading: bool
  };

  static propTypes = {
    pages: array.isRequired,
    header: element.isRequired,
    body: element.isRequired,
    location: locationShape.isRequired
  }

  constructor (props, context) {
    super(props, context);

    this.state = {
      isLoading: context.isLoading,
      class: props.routes[props.routes.length - 1].class
    };
  }

  getChildContext() {
    return {prefix: this.props.route.locale};
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
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

  componentWillReceiveProps(props, context) {
    this.setState({
      isLoading: context.isLoading,
      class: props.routes[props.routes.length - 1].class
    });
  }

  render() {
    const
      state = this.state,
      {routes, header, body, location, pages} = this.props,
      route = routes[routes.length - 1],
      aboutPage = pages.find(p => p.alias === 'about'),
      contactsPage = pages.find(p => p.alias === 'contacts'),
      langs = route.getLangs && route.getLangs() || config.locales
      ;
      // <ReactCSSTransitionGroup transitionName="loader" transitionEnterTimeout={200} transitionLeaveTimeout={200}>
      //   {state.isLoading && <Loader key="loader" />}
      // </ReactCSSTransitionGroup>
    return (
        <div id="main" className={state.class} ref="main">
          <Animate
            component=""
            transitionName="loader"
            showProp="visible"
            onEnter={(k) => console.log('enter', k)}
            onLeave={(k) => console.log('leave', k)}
          >
          <Loader key="loader" visible={state.isLoading} />
          </Animate>
          {header}
          <div className="content" ref="content">
            {body}
            <footer>
              <Link to="/" >{config.title}</Link> | &copy; <FormattedMessage
                    id="footer"
                    defaultMessage={`2016, Artem Puchenkin`}
                />
              {aboutPage && contactsPage.title && [" | ", <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
              {contactsPage && contactsPage.title && [" | ", <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
              <Langs location={ location } langs={langs} />
            </footer>
          </div>
        </div>
      );
  }
}

export default withRouter(Main);
