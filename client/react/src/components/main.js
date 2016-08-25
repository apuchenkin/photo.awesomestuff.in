import React from 'react';
import Link from 'react-router/lib/Link';

import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

var isBrowser = (typeof window !== 'undefined');
var Ps = isBrowser ? window.Ps || require('perfect-scrollbar') : null;

class Main extends React.Component {
    constructor (props) {
      super(props)

      this.state = {
        class: props.routes[props.routes.length - 1].class
      }
    }

  	componentDidMount() {
  		const
        me = this,
        props = me.props
      ;

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
        state = this.state
      ;

      if(this.props.children) {
        debugger;
      }

      return (
        <div id="main" className={state.class} ref="main">
          {this.props.header}
          <div className="content" ref="content">
            {this.props.body}
            <footer>
              <Link to="/">photo.awesomestuff.in</Link> | © 2015, Пученкин Артём | <Link to="/about">О сайте</Link> | <Link to="/contacts">Контакты</Link>
            </footer>
          </div>
        </div>
      );
    }
}

// Main.contextTypes = {
//   initialState: React.PropTypes.any.isRequired
// };

export default Main;
