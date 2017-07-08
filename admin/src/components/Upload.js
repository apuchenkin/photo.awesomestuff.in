import React from 'react';
import classNames from 'classnames';
import Immutable from 'seamless-immutable';
import { Observable } from 'rxjs/Observable';
import { Subscriber } from 'rxjs';
import 'rxjs/add/observable/of';
import { DropTarget } from 'react-dnd';
import { NativeTypes } from 'react-dnd-html5-backend';
import { ajax } from 'rxjs/observable/dom/ajax';
import authService from '../service/auth';
import config from '../../etc/config';

const fileTarget = {
  drop(props, monitor, cmp) {
    cmp.onDropFiles(monitor.getItem().files);
  },
};

const collectDrop = (connect, monitor) => ({
  dropTarget: connect.dropTarget(),
  hovered: monitor.isOver() && monitor.canDrop(),
  drop: monitor.getDropResult(),
});

const STATUS_PENDING = Symbol('pending');
const STATUS_COMPLETE = Symbol('complete');
const STATUS_ERROR = Symbol('error');

const File = ({ file: { file, progress, status, error } }) => (
  <li className="upload-file">
    <span className="title">{file.name}</span>
    {progress && (
      <span className="progress">
        <span className="bar" style={{ width: `${progress}%` }} />
      </span>
    )}
    <span className="status">
      <i className="material-icons" title={error} >
        {
          ({
            [STATUS_PENDING]: 'pause',
            [STATUS_COMPLETE]: 'check_circle',
            [STATUS_ERROR]: 'error',
          }[status])
        }
      </i>
    </span>
  </li>
);

class Upload extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      files: Immutable([]),
    };

    this.updateFile = this.updateFile.bind(this);
    this.onDropFiles = this.onDropFiles.bind(this);
  }

  onDropFiles(files) {
    const files$ = files.map((file) => {
      const body = new FormData();
      body.append('file', file);

      ajax({
        method: 'POST',
        url: `${config.apiEndpoint}/upload/${this.props.category.name}`,
        body,
        headers: {
          'Content-Disposition': `attachment; filename="${file.name}"`,
          Authorization: authService.getToken(),
        },
        progressSubscriber: Subscriber.create(e =>
          this.updateFile(file, { progress: Number(100 * (e.loaded / e.total)) }),
          error => Observable.of(this.updateFile(file, { status: STATUS_ERROR, error })),
          () => this.updateFile(file, { status: STATUS_COMPLETE }),
        ),
      })
      .catch(() => Observable.empty)
      .subscribe();

      return Immutable({ file, status: STATUS_PENDING });
    });

    this.setState({ files: Immutable(files$) });
  }

  updateFile(file, data) {
    this.setState(state => ({
      files: state.files.update(
        state.files.findIndex(f => f.file === file),
        f => f.merge(data),
      ),
    }));
  }

  render() {
    const { dropTarget, children, hovered } = this.props;
    const { files } = this.state;

    return dropTarget(
      <div className={classNames('upload', { hovered })}>
        { files.length ? (
          <ul>
            {files.map(file => <File file={file} key={file.file.name} />)}
          </ul>
        ) : children }
      </div>,
    );
  }
}

export default DropTarget(NativeTypes.FILE, fileTarget, collectDrop)(Upload);
