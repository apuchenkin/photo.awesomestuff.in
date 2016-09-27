import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import { locationShape } from 'react-router/lib/PropTypes';
import withRouter from 'react-router/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Component from '../lib/PureComponent';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import LanguageSwitcher from './common/LanguageSwitcher';
import Link from './link';
import Loader from './loader/loader';
import config from '../config/config.json';

const
  { string, func, element, arrayOf, shape, object } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Ps = isBrowser ? require('perfect-scrollbar') : null
  ;

const messages = defineMessages({
  footer: {
    id: 'footer',
    defaultMessage: '2016, Artem Puchenkin',
  },
});

const Footer = ({ aboutPage, contactsPage, langs, location }) =>
  <footer>
    <Link to="/" >{config.title}</Link> | &copy; <FormattedMessage {...messages.footer} />
    {aboutPage && contactsPage.title && [' | ', <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
    {contactsPage && contactsPage.title && [' | ', <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
    <LanguageSwitcher location={location} langs={langs || config.locales} />
  </footer>
;

Footer.propTypes = {
  aboutPage: shape({ title: string.isRequired }),
  contactsPage: shape({ title: string.isRequired }),
  langs: arrayOf(object),
  location: locationShape.isRequired,
};


@withStyles(style, baseStyle)
@withRouter
export default class Main extends Component {

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

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  render() {
    console.log('render: main');
    const
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
          <Footer
            aboutPage={aboutPage}
            contactsPage={contactsPage}
            langs={langs}
            location={location}
          />
        </div>
        <Loader key="loader" />
      </div>
    );
  }
}
