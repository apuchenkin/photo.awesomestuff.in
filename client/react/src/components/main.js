import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import { FormattedMessage } from 'react-intl';
import { locationShape } from 'react-router/lib/PropTypes';
import withRouter from 'react-router/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

// TODO: include PS styles
// import ps from 'perfect-scrollbar/dist/css/perfect-scrollbar.css';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Langs from './common/langs';
import Link from './link';
import Loader from './loader/loader';
import config from '../config/config.json';

const
  { bool, string, func, element, arrayOf, shape } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Ps = isBrowser ? require('perfect-scrollbar') : null
  ;

class Main extends React.Component {

  static contextTypes = {
    isLoading: bool,
  };

  static propTypes = {
    routes: arrayOf(shape({
      class: string,
      getLangs: func,
    })).isRequired,

    pages: arrayOf(shape({
      alias: string.isRequired,
      title: string,
    })).isRequired,

    header: element.isRequired,
    body: element.isRequired,
    location: locationShape.isRequired,
  }

  componentDidMount() {
    const me = this;

    Ps.initialize(me.content);
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  render() {
    const
      { isLoading } = this.context,
      { routes, header, body, location, pages } = this.props,
      route = routes[routes.length - 1],
      aboutPage = pages.find(p => p.alias === 'about'),
      contactsPage = pages.find(p => p.alias === 'contacts'),
      langs = route.getLangs && route.getLangs()
      ;

    return (
      <div className={[style.main, style[route.class]].join(' ')}>
        {header}
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {body}
          <footer>
            <Link to="/" >{config.title}</Link> | &copy; <FormattedMessage
              id="footer"
              defaultMessage={'2016, Artem Puchenkin'}
            />
            {aboutPage && contactsPage.title && [' | ', <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
            {contactsPage && contactsPage.title && [' | ', <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
            <Langs location={location} langs={langs || config.locales} />
          </footer>
        </div>
        <Loader key="loader" visible={isLoading || false} />
      </div>
    );
  }
}

export default withStyles(style, baseStyle)(withRouter(Main));
