import * as React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import { NativeTypes } from 'react-dnd-html5-backend';
import compose from 'ramda/es/compose';
import { PhotoContext, ServiceContext } from '@app/context';
// import R from 'ramda';

const fileTarget = {
  drop: ({ upload, category, setFiles }, monitor) => {
    const files$ = monitor.getItem().files.map((file: File) => {
      return upload(file, category);
    });

    setFiles(monitor.getItem().files);

    Promise.all(files$).then(setFiles)
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

const File = ({ file }) => (
  <li className="upload-file">
    <span className="title">{file.name}</span>
    {console.log(file)}
    {/* {progress && (
      <span className="progress">
        <span className="bar" style={{ width: `${progress}%` }} />
      </span>
    )} */}
    <span className="status">
      {/* <i className="material-icons" title={error} >
        {
          ({
            [STATUS_PENDING]: 'pause',
            [STATUS_COMPLETE]: 'check_circle',
            [STATUS_ERROR]: 'error',
          }[status])
        }
      </i> */}
    </span>
  </li>
);

const Upload = ({ files, dropTarget, children, hovered }) => {
  return dropTarget(
    <div className={classNames('upload', { hovered })}>
      { files.length ? (
        <ul>
          {files.map(file => <File file={file} key={file.name} />)}
        </ul>
      ) : children }
    </div>,
  );
}

export default compose(
  (cmp: React.ComponentType<any>) => (props: any) => {
    const [files, setFiles] = React.useState([]);
    const { photoService } = React.useContext(ServiceContext);

    return React.createElement(cmp, {
      ...props,
      files,
      setFiles,
      upload: photoService.upload,
    });
  },
  DropTarget(NativeTypes.FILE, fileTarget, collectDrop),
)(Upload);

